# Setting up sentry-cli

Contact the Keyman team if you need access to sentry.keyman.com for development.
You will also need to install [sentry-cli](https://docs.sentry.io/cli/installation/) for uploading Debug symbols.
After setting up your personal [Auth token](http://keyman.sentry.io/settings/account/api/auth-tokens/), add the following to **~/.bashrc**

```bash
export SENTRY_AUTH_TOKEN={your Sentry auth token}
export SENTRY_URL=https://sentry.io
export SENTRY_ORG=keyman
# select the appropriate project here:
export SENTRY_PROJECT=keyman-android
```

To validate your configuration, for example for keyman-android, from the `android/` folder run `sentry-cli info`.
