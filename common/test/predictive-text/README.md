Predictive text testing CLI
===========================

Provides a command line interface to try out predictive text models.

Install
-------

**NOTE**: Requires Node >= 10.0

Install locally with `npm`:

    npm install

You can install globally like so:

    npm install -g .

Usage
-----

Start it like so:

    $ ./index.js -f path/to/model.js  # local
    $ lmlayer-cli . -f path/to/model.js  # global

You will be presented with a prompt. Type any phrase in the model's
language.

 * Press <kbd>Tab</kbd> to start selecting a suggestion.
 * Press <kbd>Tab</kbd> again to select the next suggestion.
 * Press <kbd>Shift</kbd>+<kbd>Tab</kbd> to select the previous suggestion.
 * Press <kbd>Enter</kbd> to accept the selected suggestion.
 * Press <kbd>Ctrl</kbd>+C or <kbd>Ctrl</kbd>+D to quit.

```
> He
[Hello] [Hey] [He he]
```

Defining the `LMPATH` environment variable
------------------------------------------

You can also load models by their model ID. For example,

    $ ./index.js example.en.model  # local
    $ lmlayer-cli example.en.model  # global

For this to work, you need to define the `LMPATH` environment variable.
The CLI will search the directory specified in `LMPATH` to find the
particular model.

For example, say you have
[lexical-models](https://github.com/keymanapp/lexical-models) cloned in
your home directory. To be able to access any of the `release` build
lexical models, export the `LMPATH` variable as follows:

```sh
export LMPATH="${HOME}/lexical-models/release"
```

This line usually goes in your shell startup file, e.g., `~/.bashrc`,
`~/.bash_profile`, or `~/.zshrc`, etc.

Then, any lexical model built in `~/lexical-models/release` can be used
in the command line interface.
