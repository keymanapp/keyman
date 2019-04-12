Predictive Text testing CLI
---------------------------

Provides a command line interface to try out predictive text models.

Install
-------

**NOTE**: Requires Node >= 10.0

Install dependencies with `npm`:

    npm install 

Usage
-----

Start it like so:

    $ node .

You will be presented with a prompt. Type any phrase in the model's
language. Press <kbd>Tab</kbd> to start choosing a prediction. Press the
left and right arrow keys to go through suggestions. Press
<kbd>Enter</kbd> to select the given suggestion. Press <kbd>Esc</kbd>.

Press <kbd>Ctrl</kbd>+D or <kbd>Ctrl</kbd>+C to quit.

    > He
    [ Hello ] [ Hey ] [ He he ]


ROADMAP
-------

 * The command line interface!
 * \-f _filename_ to load a model from a file
 * `LMPATH` environment variable to load a named model by id: e.g,.
   `node . example.crk.wordlist` will load the given model,
   provided `LMPATH` is defined.
