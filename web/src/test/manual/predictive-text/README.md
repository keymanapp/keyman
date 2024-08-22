Predictive text testing CLI
===========================

Provides a command line interface to try out predictive text models.

Install
-------

**NOTE**: Requires Node >= 10.0

First, ensure that Keyman web and the LMLayer are built. You can run the
build script in `/web` to do this for you:

    ../../../../web/build.sh

Then, you can install locally with `npm`:

    npm install

Or you can install globally like so:

    npm install -g .

When installed globally, you can invoke the CLI using the `lmlayer-cli`
command.

Usage
-----

### Interactive mode

Start it in interactive mode like so:

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

### Batch mode

Instead of using `lmlayer-cli` interactively, you can give the model one
or more phrases in batch mode. Either pass in a newline-separated file
with `-i`, or one or more phrases with `-p`. You can also pipe input to
`lmlayer-cli`, and it will read each line as a phrase.

```sh
$ lmlayer-cli author.bcp47.uniq -p "a test phrase" -p "another test phrase"
$ lmlayer-cli author.bcp47.unq -i my-test-file.txt
$ echo "anything from stdin" | lmlayer-cli author.bcp47.uniq
```

The output is tab-separated, with the first column being the input, and
the remaining columns being the suggestions, e.g.,

```sh
$ lmlayer-cli example.en.wordlist -p "d"
d      dinosaur        dumbo octopus
$ echo "c" | "lmlayer-cli example.en.wordlist
c      cat     cheetah
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
