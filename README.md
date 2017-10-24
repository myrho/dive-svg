# Dive SVG

Dive SVG is written in [Elm](http://elmlang.org) makes visual presentations like [Prezi](https://prezi.com)'s possible with plain SVG. 

If you are interested in an approach based on HTML5 Canvas, check out [Dive](https://github.com/myrho/dive).

## Demo

This [Web tool](https://myrho.github.io/dive-svg/) is powered by this library and gets your SVG file rendered as a visual presentation in the browser (works best in Chrome).

## Installation

First, clone this project.

You need [elm-install](https://github.com/gdotdesign/elm-github-install) to build this project since it uses non-published elm packages. 

Then run

    elm-install
    elm make src/Main.elm --output elm.js

Navigate your browser to the directory where you cloned this project and append `?samples/demo.svg` to its URL. For example:

    http://localhost/dive-svg/?samples/demo.svg

This runs **Dive SVG** and loads the sample presentation contained in this project from the file `samples/demo.svg`.

## Internals

Dive SVG exposes a [Parser](https://github.com/myrho/dive-svg/blob/master/src/DiveSvg/Parser.elm) module which turns an SVG file into Dive SVG's [Model](https://github.com/myrho/dive-svg/blob/master/src/DiveSvg/Model.elm). Groups of red rectangles and a number are recognized as frames. This model then contains a function `slides : Frame -> Svg msg` which renders the SVG with viewbox parameters given by the Frame.

However, for your application you might only need to incorporate this library's `init`, `update` and `view` functions. The latter draws the SVG element at full width and height now, hard coded.

## License

BSD-3
