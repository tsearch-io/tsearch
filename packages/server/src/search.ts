import path from 'path'
import { homedir } from 'os'

import express, { Router } from 'express'

import { getTypes, getTypesSync } from './utils'
import { search } from './search/index'

// TODO: get from config
const $home = homedir()
const p = path.join($home, '.ts-earch/types.json')
let types = getTypesSync(p)

const app = express()
const router = Router()

router.get('/', (req, res) => {
  const result = search(types)(req.query.query)

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
