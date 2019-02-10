import path from 'path'
import { homedir } from 'os'

import express, { Router } from 'express'

import { FunctionRecord } from 'ts-earch-types'
import { search } from 'ts-earch-search'

import { getTypes, getTypesSync } from './utils'
import conf from './config'

import firebase from './firebase'

// TODO: path
const admin = firebase(path.resolve('../..', conf.secretsPath))

const collection = admin.firestore().collection('queries')

// TODO: create firebase or analytics module to handle this
const addQuery = (query: string, results: FunctionRecord[]) =>
  collection
    .doc()
    .set({
      query,
      results: results.slice(0, 5).map(({ module, name, location }) => ({
        module,
        name,
        path: location.path,
      })),
    })
    .then(doc => {
      console.log('Saved doc', doc)
    })
    .catch(err => {
      console.log('Failed to save doc', err)
    })

// TODO: expand
const $home = homedir()
const p = path.join($home, conf.typesPath)
let types = getTypesSync(p)

const app = express()
const router = Router()

router.get('/', (req, res) => {
  const result = search(types)(req.query.query)

  res.json({
    data: result,
    count: result.length,
  })

  if (conf.isProd) {
    addQuery(req.query.query, result)
  }
})

router.get('/all', (req, res) => res.json(types))

router.get('/reload', (req, res) => {
  getTypes(p).fork(
    () => res.send(500),
    result => {
      types = result
      res.send(200)
    },
  )
})

app.use('/search', router)

export default app
