// ==UserScript==
// @name        narou-vertical
// @namespace   https://ncode.syosetu.com
// @include     https://ncode.syosetu.com/*
// @version     1
// /==UserScript==

// It seems like we have to hard code css for now

var css = document.createElement("style");
css.textContent = `
:root > body > *:not(#novel_header):not(#container),
:root #container > *:not(.contents1):not(#novel_contents):not(#recommend):not(#contents_main),
:root #novel_color ~ *:not(#novel_hyouka):not(#impression):not(#review):not(#novel_footer)
{
    display: none;
}

:root {
    overflow: visible;
    font-size: 14pt;
}

body {
    color: #d8d8d8;
    background-color: #000000;
}

.contents1, .nothing {
    background-color: inherit;
}

.novel_view {
    overflow: visible;
}

[> vertical mode <]
:root #container {
    overflow-y: auto;
    margin: 0;
    padding: 50px 0 0;
    height: calc(100vh - 50px);
}

:root #novel_color {
    margin: auto;
    width: auto;
    color: inherit;
}

:root #novel_ex, :root .index_box,
:root .novel_writername, :root .novellingindex_bookmarker_no {
    margin: 20px auto;
    width: 730px;
}

:root #novel_honbun {
    margin: 0 auto;
    padding: 0;
    width: calc(20rem * 1.8);
    max-width: calc(100% - 3rem);
    height: 38rem;
    max-height: calc(100vh - 50px);
    column-span: all;
    column-width: 24rem;
    column-gap: 5em;

    writing-mode: vertical-rl;
    text-combine-upright: digit 2;
    font-size: 1rem !important;
    line-height: 1.8 !important;
}

:root #novel_header {
    position: fixed;
    left: -15px;
    opacity: 0;
    transition: all .3s ease-in-out;
    -webkit-transition: all .3s ease-in-out;
}

:root #novel_header:hover {
    left: 0;
    opacity: 1;
}

:root .novel_bn:last-child {
    position: fixed;
    left: -15px;
    bottom: 0;
    margin: 0 auto;
    width: 100%;
    opacity: 0;
    transition: all .3s ease-in-out;
    -webkit-transition: all .3s ease-in-out;
}

:root .novel_bn:last-child:hover {
    left: 0;
    opacity: 1;
    border-top: 1px solid #aaa;
    background-color: #fff;
}

#novel_p, #novel_a, #novel_hyouka, #impression, #review,
#novel_footer, #recommend {
    position: fixed;
    left: 0;
    z-index: 15;
    overflow: hidden;
    width: 0;
    height: 8vh;
    margin: 0;
    padding: 0 0 0 1vw;
    border: none;
    background: hsla(0, 0%, 100%, .9) no-repeat left top / 1vw 8vh;
}

#novel_p:hover, #novel_a:hover, #novel_hyouka:hover, #impression:hover,
#review:hover, #novel_footer:hover, #recommend:hover {
    overflow-y: auto;
    padding: 1em 2em;
    width: 50vw;
    max-width: 80vw;
    height: calc(100vh - 2em);
    top: 0;
    z-index: 10;
}

#novel_p {
    top: calc(50px + 13vh * 0);
    background-image: linear-gradient(0deg, hsl(calc(0 * 45), 87%, 68%), hsl(calc(0 * 45), 87%, 68%));
}

#novel_p::before {
    display: block;
    content: '- ???????????? -';
}

#novel_a {
    top: calc(50px + 13vh * 1);
    background-image: linear-gradient(0deg, hsl(calc(1 * 45), 87%, 68%), hsl(calc(1 * 45), 87%, 68%));
}

#novel_a::before {
    display: block;
    content: '- ???????????? -';
}

#novel_hyouka {
    top: calc(50px + 13vh * 2);
    background-image: linear-gradient(0deg, hsl(calc(2 * 45), 87%, 68%), hsl(calc(2 * 45), 87%, 68%));
}

#impression {
    top: calc(50px + 13vh * 3);
    background-image: linear-gradient(0deg, hsl(calc(3 * 45), 87%, 68%), hsl(calc(3 * 45), 87%, 68%));
}

#review {
    top: calc(50px + 13vh * 4);
    background-image: linear-gradient(0deg, hsl(calc(4 * 45), 87%, 68%), hsl(calc(4 * 45), 87%, 68%));
}

#novel_footer {
    top: calc(50px + 13vh * 5);
    background-image: linear-gradient(0deg, hsl(calc(5 * 45), 87%, 68%), hsl(calc(5 * 45), 87%, 68%));
}

#recommend {
    top: calc(50px + 13vh * 6);
    background-image: linear-gradient(0deg, hsl(calc(6 * 45), 87%, 68%), hsl(calc(6 * 45), 87%, 68%));
}
`;
document.head.appendChild(css);

