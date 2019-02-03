import express from 'express'

import bodyParser from 'body-parser'
import morgan from 'morgan'
import cors from 'cors'

import conf from './config'
import search from './search'

const app = express()

app.use(cors())
app.use(bodyParser.json())
app.use(bodyParser.urlencoded({ extended: true }))
app.use(morgan('tiny'))

app.use(search)

app.use('/', (req, res) => {
  res.json({ message: 'Welcome to ts-earch!' })
})

app.listen(conf.port, (err?: Error) => {
  if (err) {
    console.error(err)
  } else {
    console.log(`Listening on localhost:${conf.port}`)
  }
})
