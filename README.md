# Dive SVG

Dive SVG is written in [Elm](http://elmlang.org) makes visual presentations like [Prezi](https://prezi.com)'s possible with plain SVG. 

If you are interested in an approach based on HTML5 Canvas, check out [Dive](https://github.com/myrho/dive).

## Demo

This [Web tool](https://myrho.github.io/dive-svg/) is powered by this library and gets your SVG file rendered as a visual presentation in the browser (works best in Chrome).

## Internals

Dive SVG exposes a [Parser](https://github.com/myrho/dive-svg/blob/master/src/DiveSvg/Parser.elm) module which turns an SVG file into Dive SVG's [Model](https://github.com/myrho/dive-svg/blob/master/src/DiveSvg/Model.elm). Groups of red rectangles and a number are recognized as frames. This model then contains a function `slides : Frame -> Svg msg` which renders the SVG with viewbox parameters given by the Frame.

However, for your application you might only need to incorporate this library's `init`, `update` and `view` functions. The latter draws the SVG element at full width and height now, hard coded.

## License

BSD-3
