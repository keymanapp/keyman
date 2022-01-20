# [node-hide-console-window](https://www.npmjs.com/package/node-hide-console-window)

A node module to toggle your app's console window visibility. (Can be used with compilers
like [pkg](https://www.npmjs.com/package/pkg))

### Platforms

- Windows

## Warning

I recently updated this package to match the javascript lint so if you are experiencing any problem related to not
finding the method take a look at the usage section again.

## Installation

```bash
yarn add node-hide-console-window
```

or

```bash
npm install node-hide-console-window
```

## Usage

#### Using import syntax

```typescript
import {showConsole, hideConsole} from "node-hide-console-window";

//To hide your console just call:
hideConsole();

//To show it again use:
showConsole();
```

#### Using require syntax

```typescript
const ConsoleWindow = require("node-hide-console-window");

//To hide your console just call:
ConsoleWindow.hideConsole();

//To show it again use:
ConsoleWindow.showConsole();
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)