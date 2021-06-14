// ==UserScript==
// @name        vertical-writebox
// @namespace   https://write-box.appspot.com/
// @include     https://write-box.appspot.com/
// @version     1
// /==UserScript==

// It seems like we have to hard code css for now
// Note that vertical writing of `textarea` is  Firefox only

var css = document.createElement("style");
css.textContent = `
/* Firefox only */

/* ----------------------------------------
   configuration
*/
:root {
    --bg: rgb(43, 47, 47);
    --fg: rgb(220, 200, 180);
    --button: rgb(200, 200, 200);
}

/* Not supported by browsers..
   maybe use greasemonkey to set `spellcheck="false"` */
::spelling-error,
::grammer-error {
    color: var(--bg) !important;
}

/* ----------------------------------------
   layout
*/
#main {
    height: 800px;

    /* set it manually... */
    margin-top: 100px;
}

textarea {
    writing-mode: vertical-rl;
    overflow-x: scroll;
    overflow-y: scroll;
    border-bottom: 1px solid rgb(180,180,180);
    
    padding: 10px 0 5px 0 !important;
    margin: auto !important;
    width: 1000px !important;

    /* NO multi column support..
    column-span: all;
    column-width: 24rem;
    column-gap: 5em;
    */
}

#chooser-dialog > .window {
    width: 1000px !important;
    height: 600px !important;

    background-color: var(--bg) !important;
    color: var(--fg);
}

.main-menu,
.side-menu {
    height: 500px !important;
}

/* ----------------------------------------
   colors
*/
body {
    background-color: var(--bg) !important;
    color: var(--fg) !important;
}

#filename,
textarea,
#document-statistics {
    color: var(--fg) !important;
}

#header {
    border: none;
    background-color: var(--bg) !important;
}

.command {
    background-color: var(--button);
    border: 1px solid var(--fg);
}
`;
document.head.appendChild(css);

