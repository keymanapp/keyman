const express = require('express')
const path = require('path')
const app = express()
const port = 3000

app.use(express.static(path.join(__dirname, '../../../../')))

// for testing timeout error in web/src/test/manual/web/keyboard-errors
const router = express.Router()
router.get('/src/test/manual/web/keyboard-errors/timeout.js', async (req, res, next) => {
  console.log('timeout.js requested')
  return new Promise(() => {
    setTimeout(() => {
      res.set('Content-Type', 'application/json');
      res.status(200);

      next();
    }, 10500); // > ContextManagerBase.TIMEOUT_THRESHOLD (10 seconds)
  });
})
app.use(router)

app.listen(port, () => {
  console.log(`Keyman test app listening on port ${port}`)
})
