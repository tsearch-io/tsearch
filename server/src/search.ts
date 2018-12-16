import fs from 'fs'
import path from 'path'
import { promisify } from 'util'

import express, { Router } from 'express'

import { FunctionRecord } from './models/tSearch'
import { searchFunctions } from './search/index'

interface Query {
  query: string
}

const readFile = promisify(fs.readFile)

// TODO: this should be configurable
const p = path.join(__dirname, '../../cli/.ts-search/functions.json')

let data: FunctionRecord[] = []

const loadDataSync = () => {
  const fileContent = fs.readFileSync(p, 'utf-8')

  data = JSON.parse(fileContent)
}

const loadData: () => Promise<void> = () =>
  readFile(p, 'utf-8').then(fileContent => {
    data = JSON.parse(fileContent)
  })

loadDataSync()

const app = express()
const router = Router()

router.get('/', (req, res) => {
  const { query } = req.query as Query
  const matcher = searchFunctions(query)

  const result = data.filter(matcher)

  res.json(result)
})

router.get('/reload', (req, res) => {
  loadData()
    .then(() => res.send(200))
    .catch(() => res.send(500))
})

app.use('/search', router)

export default app
