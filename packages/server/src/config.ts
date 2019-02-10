const env = process.env.NODE_ENV || 'development'

const conf = {
  port: process.env.PORT || '8080',
  typesPath: process.env.TYPES_PATH || '.ts-earch/types.json',
  secretsPath: process.env.SECRETS_PATH || './firebase.json',
  isProd: env === 'production',
  env,
}

export default conf
