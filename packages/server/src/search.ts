import path from 'path'

import express, { Router } from 'express'

import { getTypes, getTypesSync } from './utils'
import { matchesQuery } from './search/index'

// TODO: get from config
const p = path.join(__dirname, '../../cli/.ts-earch/functions.json')
let types = getTypesSync(p)

const app = express()
const router = Router()

router.get('/', (req, res) => {
  const result = types.filter(matchesQuery(req.query.query))

  res.json(result)
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
