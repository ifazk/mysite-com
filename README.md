# Requirements
- stack
- zeromq
- node.js/npm and the katex npm package

You may skip the KaTeX steps if you do not want to use KaTeX.

# Build 
To setup hakyll

    stack init

To setup KaTeX

    cd katex-zmq-ipc && npm install

Modify `templates/default.html` and add correct name in window title.
Modify `templates/nav.html` and add correct urls for contact information.
Modify `Makefile` and add correct `rsync` details.
Modify `images/pic.jpg` and `images/favicon.ico`.

# Site Generation
To build the site, you must have a `katex-zmp-ipc` process running.

    cd katex-zmq-ipc
    npm start &

Then start up `hakyll watch`
    make

Your static site will be available at localhost:8000.

To kill `hakyll watch`
    CTRL + C

To kill `katex-zmq-ipc`
    fg
    CTRL + C

To publish via rsync
    make publish

# Front matter options when writing markdown
- `title`: string
- `subtitle`: string
- `katex`: boolean, adds the katex css to the page and processes latex with katex
- `tags`: comma separated strings
- `no-window-title`: boolean, used in index.html to avoid hyphen in the window title
