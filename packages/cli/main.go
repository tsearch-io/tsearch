package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"time"
)

var (
	bin = flag.String("bin", "./packages/cli/bin/ts-earch", "ts-earch bin path")
	out = flag.String("out", "~/.ts-earch", "out file path")
	dt  = flag.String("dt", "~/dev/DefinitelyTyped/types", "DefinitelyTyped types/ path")
	x   = flag.Int("x", 0, "only extract the first X modules (extracts all when 0)")
)

// Modules is just an slice Module's.
type Modules []Module

func (ts *Modules) append(xs ...Module) {
	*ts = append(*ts, xs...)
}

// Param represents a parameter of a function.
type Param struct {
	Name      string `json:"name"`
	Type      string `json:"type"`
	IsGeneric bool   `json:"isGeneric"`
}

// Position represents the lines where piece of code is located in a file.
type Position struct {
	From int `json:"number"`
	To   int `json:"to"`
}

// Location represents where a piece of code is located.
type Location struct {
	Path  string   `json:"path"`
	Lines Position `json:"lines"`
}

// FunctionRecord contains all the information about a TS function.
type FunctionRecord struct {
	Name       string   `json:"name"`
	Docs       string   `json:"docs"`
	Text       string   `json:"text"`
	Parameters []Param  `json:"parameters"`
	ReturnType string   `json:"returnType"`
	Location   Location `json:"location"`
	Module     string   `json:"module"`
}

// Module contains all the exported functions of a module.
type Module struct {
	Name string           `json:"name"`
	Fns  []FunctionRecord `json:"fns"`
}

func execAndAppend(dir string, acc *Modules, mutex *sync.Mutex) error {
	ts, err := execOut(*bin, dir)
	if err != nil {
		return err
	}

	module := Module{Name: dir}

	err = json.Unmarshal(ts, &module.Fns)
	if err != nil {
		return err
	}

	mutex.Lock()
	defer mutex.Unlock()

	acc.append(module)

	return nil
}

func main() {
	flag.Parse()
	expand(bin)
	outPath(out)
	expand(dt)

	runtime.GOMAXPROCS(runtime.NumCPU())
	var n sync.WaitGroup
	var fetchSema = make(chan struct{}, runtime.NumCPU())
	var mutex = &sync.Mutex{}

	dirs, err := ioutil.ReadDir(*dt)
	check(err)

	modules := Modules{}
	count := 0
	if *x != 0 {
		dirs = dirs[:*x]
	}

	fmt.Printf("Running ts-earch-cli. Worker count: %v\n", runtime.NumCPU())

	started := time.Now()
	for _, dir := range dirs {
		if !dir.IsDir() {
			continue // only directories
		}
		fetchSema <- struct{}{} // adquire token
		n.Add(1)
		go func(dir os.FileInfo) {
			extractStarted := time.Now()

			defer n.Done()
			defer func() {
				<-fetchSema // release token
				count++
				fmt.Printf("Done %v/%v (%s took %s)\n",
					count, len(dirs), dir.Name(), time.Since(extractStarted))
			}()

			fmt.Printf("Extracting %s\n", dir.Name())
			path := filepath.Join(*dt, dir.Name())

			err = execAndAppend(path, &modules, mutex)
			if err != nil {
				fmt.Printf("Failed to extract %s\n", dir.Name())
				fmt.Println(err.Error())
			}
		}(dir)
	}

	n.Wait()
	close(fetchSema)

	encoded, err := json.Marshal(modules)
	check(err)

	f, err := os.OpenFile(*out, os.O_RDWR|os.O_CREATE, 0755)
	check(err)
	defer f.Close()

	_, err = f.Write(encoded)
	check(err)

	fmt.Printf("Wrote %s\nTook %s\n", *out, time.Since(started))
}

func check(err error) {
	if err != nil {
		panic(err.Error())
	}
}

func execOut(command string, args ...string) ([]byte, error) {
	out, err := exec.Command(command, args...).Output()
	if err != nil {
		return []byte{}, err
	}

	return bytes.TrimSpace(out), nil
}

func expand(p *string) {
	usr, err := user.Current()
	check(err)
	dir := usr.HomeDir

	if *p == "~" {
		*p = dir
	} else if strings.HasPrefix(*p, "~/") {
		*p = filepath.Join(dir, (*p)[2:])
	}
}

func outPath(base *string) {
	expand(base)

	f, err := os.Stat(*base)
	if err != nil {
		panic(err.Error())
	}

	if !f.IsDir() {
		panic("--out should be a directory")
	}

	*base = filepath.Join(*base, time.Now().UTC().Format("2006-01-02-03-04-05.json"))
}
