import * as admin from 'firebase-admin'

const firebase = (secretsPath: string) => {
  const serviceAccount = require(secretsPath)

  admin.initializeApp({
    credential: admin.credential.cert(serviceAccount),
    databaseURL: 'https://tsearc-analytics.firebaseio.com',
  })

  return admin
}

export default firebase
