Readme
======

This is a Lucidity instance of my professional website.

Currently, the site uses Lucidity to add JS to a single HTML file: index.html.

Modifying index.html
--------------------

1. modify index-mustache-vars.js
2. modify index-mustache-template.js
3. compile generate index.html using:
   `~/node_modules/mustache/bin/mustache index-mustache-vars.js index-template.mustache index.html`
   note: you can install mustache using `npm install mustache`

Modifying the CSS
-----------------

1. modify themes/portfolio/portfolio.scss
2. modify themes/base/base.scss (and other module files as needed)
3. compile the CSS using `sass portfolio.scss > portfolio.css`

Configuring the Lucidity modules
--------------------------------

To set the Analytics tracker, etc, view the Lucidity Readme.md file.
