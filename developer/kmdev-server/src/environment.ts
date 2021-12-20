
export enum Environment {
  Development = 'development',
  Release = 'release'
};

export const environment: Environment = Environment.Development; // TODO: lookup env var KEYMAN_ROOT and only use Development if in that path
/*
process.execPath
  process.env['NODE_ENV'] == 'production' ? Environment.Production :
  process.env['NODE_ENV'] == 'staging' ? Environment.Staging :
  Environment.Development;
*/
