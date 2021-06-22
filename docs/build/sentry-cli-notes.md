# Setting up sentry-cli

Contact the Keyman team if you need access to sentry.keyman.com for development. 
You will also need to install [sentry-cli](https://docs.sentry.io/cli/installation/) for uploading Debug symbols. 
After setting up your personal [Auth token](http://sentry.keyman.com/settings/account/api/auth-tokens/), add the following to **~/.bashrc**

```bash
export SENTRY_AUTH_TOKEN={your Sentry auth token}
export SENTRY_URL=https://sentry.keyman.com
export SENTRY_ORG=keyman
export SENTRY_PROJECT=keyman-android
```

To validate your configuration, from the `android/` folder run `sentry-cli info`.
