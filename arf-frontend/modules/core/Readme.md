Core Module
===========

The Core module represents Lucidities core system. It is responsible for loading the system's configuration settings and all of the other modules that comprise the complete system.

This module also defines the core system API which is used by all of the other modules.

```javascript
/*
  The Core module represents Lucidities core
  system. It is responsible for loading the
  system's configuration settings and all of
  the other modules that comprise the complete
  system.
*/

// Declares the QUnit test module.
QUnit.module ('Core');
```

Running and Displaying Unittests
--------------------------------

Lucidity comes with unittests. To run the tests, set the 'runUnittests' value in settings.xml to 'true'.

In order to display the tests' output, you must insert two div elements with the ids of "qunit" and "qunit-fixture", respectively. They must be explicitly embedded in index.html, NOT inserted with a template, so that they exist when QUnit starts.

Paste the following lines into guides/eoffer/index.html, inside of the element with the id of "main_content":

```html
<div id="qunit"></div>
<div id="qunit-fixture"></div>
```

guides/eoffer/index.html also needs to load the QUnit library, and ensure that QUnit's autostart value is set to false. To do so, paste the following inside of the head element, below the other script elements:

```javascript
<script src="lib/qunit/qunit.js"></script>
<script>
  QUnit.config.autostart = false;
</script>
```

Global Variables
----------------

The Core module defines a number of global variables. These variables can be divided into two groups: those that specify global configuration parameters and those that list registered block and page handlers. 

The Core module defines two global configuration parameters:

* `SETTINGS_URL`

  `SETTINGS_URL` specifies the location of the Configuration Settings file.

* `STRICT_ERROR_MODE`

  `STRICT_ERROR_MODE` indicates whether or not Lucidity should try to recover gracefully from errors or report them immediately. This parameter is set by the `errorMode` element in the Configuration Settings file. 

The remaining global variables store the registered block and page handlers. Whenever a module defines a block or page handler, the module's load event handler must register the block or page handler by calling `registerBlockHandler` or `registerPageHandler` respectively. These functions add entries to `BLOCK_HANDLERS` and `PAGE_HANDLERS`.

Both variables are associative arrays keyed by name. See "Page Handlers" and "Block Handlers" below for more details about page and block handlers.

```javascript
// Specifies the settings file URL. 
var SETTINGS_URL = 'settings.xml';

/*
  The global STRICT_ERROR_MODE variable indicates
  whether or not the system should exit and return
  an error message or try to recover silently from
  non-critical errors. This variable is set by the
  "errorMode" parameter in settings.xml.
*/
var STRICT_ERROR_MODE = true;

/*
  The global currentId variable is used to
  generate unique HTML IDs for HTML elements
  created by this module's `getUniqueId`
  function.
*/
var currentId = 0;
```

The Module Load Handlers Store Class
------------------------------------

The Core module is responsible for loading all of the other modules within Lucidity. These modules may optionally register a callback function called a Load Event Handler that will be called immediately after the module is loaded.

These Load Event Handlers are stored in the `MODULE_LOAD_HANDLERS` global variable which is a `ModuleLoadHandlers` store class.

```javascript
/*
  Module Load Handler Stores store the loaded
  module initialization functions - I.E. Load
  Event Handlers.
*/
function ModuleLoadHandlers () {
  // A Module Load Handler array.
  var _handlers = [];

  /*
    Accepts one argument: handler, a function
    that accepts one argument, next; adds handler
    to this store as a Load Event Handler;
    and returns undefined.

    Note: next is a function that accepts one
    argument: error, an error string. handler
    must call next when done and may pass an
    optional error string to next if an error
    occured.
  */
  this.add = function (handler) { _handlers.push (handler); }

  /*
    Accepts one argument, done, calls every
    Load Event Handler function in this store,
    and calls done.

    Done is a function that accepts two
    arguments:

    * error, an error string
    * and results, an array.

    If any of the Load Event Handlers passes an
    error string to their continuation function,
    next, this function passes the error string
    to done without executing the remaining
    handlers.
  */
  this.execute = function (done) {
    async.series (_handlers, done);
  }
}

/*
  Unittests for ModuleLoadHandlers.

  Confirms that:

  * ModuleLoadHandlers.execute will execute all
    functions added using ModuleLoadHandlers.add
  * ModuleLoadHandlers.execute will return any
    error messages generated by a handler without
    executing the remaining handlers.
*/
QUnit.test ('ModuleLoadHandlers', function (assert) {
  assert.expect (3);

  /*
    I. Confirm that ModuleLoadHandlers.execute
       will execute all functions added using
       ModuleLoadHandlers.add.
  */
  {
    var store = new ModuleLoadHandlers ();

    var x = { value: 5 };
    store.add (function (next) { x.value ++; next (); });
    store.add (function (next) { x.value *= 2; next (); });

    var done = assert.async (); 
    store.execute (function (error) {
      assert.strictEqual (x.value, 12, 'ModuleLoadHandlers.execute executed the functions that were added to the store using ModuleLoadHandlers.add in the correct order.');
      done ();
    });
  }
  /*
    II. Confirm that ModuleLoadHandlers.execute
        will execute return any error messages
        generated by a handler without executing
        the remaining handlers.
  */
  {
    var store = new ModuleLoadHandlers ();

    var x = 'initial';
    store.add (function (next) { next ('error'); });
    store.add (function (next) { x = 'modified'; next (); });

    var done = assert.async ();
    store.execute (function (error) {
      assert.strictEqual (error, 'error', 'ModuleLoadHandlers.execute correctly returned an error when one of the handlers generated an error.');
      assert.strictEqual (x, 'initial', 'ModuleLoadHandlers.execute did not execute the remaining handlers after an error was generated.');
      done ();
    });
  }
});
```

The Module Load Handlers Store
------------------------------

```javascript
/*
  Creates a ModuleLoadHandlers object that stores
  the registered module Load Event Handlers.
*/
var MODULE_LOAD_HANDLERS = new ModuleLoadHandlers ();
```

The App Load Handlers Store Class
---------------------------------

When loaded, modules may optionally register App Load Event Handlers. Once the Core module has loaded all of the enabled modules, it then calls these handler functions.

These App Load Event Handlers are stored in the `APP_LOAD_HANDLERS` global variable which is an instance of the `AppLoadHandlers` store class.

```javascript
/*
  App Load Handler Stores store the loaded
  app initialization functions.
*/
function AppLoadHandlers () {
  // Create an empty array that will hold App Load Handlers
  var _handlers = [];

  /*
    Accepts one argument: handler, an App Load Handler. Pushes that handler to this store.
  */
  this.add = function (handler) { _handlers.push (handler); }

  /*
    Accepts two arguments:
    * settings, a Settings object
    * done, a function
    Calls all of the App Load Handlers in the store, passes the settings to the load handlers, and calls done.
  */
  this.execute = function (settings, done) {
    async.applyEachSeries (_handlers, settings, done);
  }
}

/*
  Unittests for AppLoadHandlers.

  Confirms that:

  * AppLoadHandlers.execute calls every function
    added using AppLoadHandlers.add sequentially
    on the given object.
  * AppLoadHandlers.execute will return an error
    without executing the remaining handlers if
    any of them generates an error.
*/
QUnit.test ('AppLoadHandlers', function (assert) {
  assert.expect (3);

  /*
    I. Confirm that AppLoadHandlers.execute calls
       every function added using AppLoadHandlers
       sequentially on the given object.
  */
  {
    var store = new AppLoadHandlers ();

    var x = { value: 5 };
    store.add (function (x, next) { x.value ++; next (); });
    store.add (function (x, next) { x.value *= 2; next (); });

    var done = assert.async ();
    store.execute (x, function (error) {
      assert.strictEqual (x.value, 12, 'AppLoadHandlers.execute called the functions that were added to the store on the correct argument and in the correct order.');
      done ();
    });
  }

  /*
    II. Confirm that AppLoadHandlers.execute
        will return an error without executing the
        remaining handlers if any of them generates
        an error.
  */
  {
    var store = new AppLoadHandlers ();

    x = { value: 0 };
    store.add (function (x, next) { next ('error'); });
    store.add (function (x, next) { x.value = 1; next (); });

    var done = assert.async ();
    store.execute (x, function (error) {
      assert.strictEqual (error, 'error', 'AppLoadHandlers.execute correctly returned an error when one if its handlers generated an error.');
      assert.strictEqual (x.value, 0, 'AppLoadHandlers.execute did not execute the remaining handlers after one of its handlers generated an error.');
      done ();
    });
  }
});
```

The App Load Handlers Store
---------------------------

```javascript
/*
  Creates an AppLoadHandlers object that stores the registered
  App Load Handlers.
*/
var APP_LOAD_HANDLERS = new AppLoadHandlers ();
```

The Load Event Handler
----------------------

The Core module's load event handler runs when the site is loaded. This function performs three operations:

1. it loads the configuration settings specified in settings.xml
2. it loads the modules listed as enabled in settings.xml and executes the Load Event Handler registered for each
3. and lastly, it executes the App Load Event Handler registered for each module.

```javascript
/*
  The Document Ready event handler. This function
  loads the modules that have been enabled in
  settings.xml and initializes the user interface
  by expanding any blocks that have been embedded
  within the current page.
*/
$(document).ready (function () {
  progressbar.update ('core.load_app', 10);

  // I. Load the configuration settings.
  loadSettings (function (settings) {
    progressbar.update ('core.load_app', 20);
    progressbar.update ('core.load_settings', 100);

    // Add the main module to the modules list.
    settings.modules.push ({
      name:    'main',
      enabled: true,
      url:     'index.js'
    });

    // II. Load the enabled modules.
    loadModules (settings, function () {
      progressbar.update ('core.load_app', 30);

      // III. Update the error mode.
      STRICT_ERROR_MODE = settings.errorMode;

      // IV. Call the module load event handlers.
      MODULE_LOAD_HANDLERS.execute (function () {
        progressbar.update ('core.load_app', 50);

        if (settings.runUnittests) {
          progressbar.update ('core.load_app', 100);

          // V. Run unit tests
          $.getCSS ('lib/qunit/qunit.css');
          QUnit.start ();
        } else {
          // V. Call the app load event handlers.
          progressbar.update ('core.load_app', 100);
          APP_LOAD_HANDLERS.execute (settings, function () {});         
        }
      });
    });
  });
});
```

Load Settings
-------------

The load event handler uses `loadSettings` to load and parse settings.xml, and return the configuration settings.

```javascript
/*
  loadSettings accepts one argument: done, a 
  function that accepts a Settings object. It 
  parses the settings.xml file and passes the 
  result to done.
*/
function loadSettings (done) {
  $.ajax (SETTINGS_URL, {
    dataType: 'xml',
    success: function (doc) {
      done (parseSettings (doc));
    },
    error: function (request, status, error) {
      throw new Error ('[core][loadSettings] Critical Error: an error occured while trying to load "' + SETTINGS_URL + '". ' + error);
    }
  });
}

/*
  parseSettings accepts one argument: doc, a JQuery
  HTML DOM Document. doc must represent a valid
  Settings document. This function parses doc and
  returns a Settings object that represents it.  
*/
function parseSettings (doc) {
  return {
    errorMode: $('errorMode', doc).text () === 'strict',
    runUnittests: $('runUnittests', doc).text () === 'true',
    theme:     $('theme', doc).text (),
    modules:   $('module', doc).map (function (moduleIndex, moduleElement) {
      return {
        name:    $(moduleElement).attr ('name'),
        enabled: $(moduleElement).attr ('enabled') === 'true',
        url:     $(moduleElement).attr ('url')
      };
    }).toArray ()
  };
}
```

Load Modules
------------

Once the configuration settings have been loaded from settings.xml, the load event handler uses `loadModules` to load the modules that have been listed as enabled within settings.xml.

```javascript
/*
  loadModules accepts two arguments: settings, a
  Settings object; and done, a function. It
  loads the modules declared in settings, and 
  calls done after they have all been loaded. If
  an error occurs while trying to load one of the
  modules, this function will throw a strict error
  and continue on to the next one.
*/
function loadModules (settings, done) {
  // II. Load the module files in the modules list.
  async.eachOfSeries (settings.modules,
    function (module, index, next) {
      progressbar.update ('core.load_modules', index/settings.modules.length * 100);

      module.enabled ? loadScript (module.url, next) : next ();
    },
    function (error) {
      progressbar.update ('core.load_modules', 100);
      done (error);
    }
  );
}

/*
  loadScript accepts two arguments:

  * url, a URL string
  * and done, a function that accepts one
    argument: error, an Error.

  It loads the script referenced by url and calls
  done. If an error occurs, this function throws
  a strict error and calls done.
*/
function loadScript (url, done) {
  $.getScript (url)
    .done (function () { done (null); })
    .fail (function (jqxhr, settings, exception) {
        var error = new Error ('[core][loadScript] Error: an error occured while trying to load "' + url + '".');
        strictError (error);
        done (error);
      });
}

/*
  loadScripts accepts two arguments:

  * urls, an array of URL strings
  * and done, a function that accepts one
    argument: error, an Error

  loads the scripts referenced by urls and calls
  done. If any of these scripts fails to load,
  this function throws a strict error and calls
  done without loading the remaining scripts.
*/
function loadScripts (urls, done) {
  async.eachSeries (urls, loadScript,
    function (error) {
      if (error) {
        strictError (new Error ('[core][loadScripts] Error: an error occured while trying to load one or more scripts. ' + error.message));
        done (error);
      }
      done (null);
  });
}
```

Auxiliary Functions
-------------------

```javascript
/*
  replaceWithTemplate accepts four arguments:

  * url, a URL string
  * element, a JQuery HTML Element
  * done, a function that accepts two arguments:
    an Error object and a JQuery HTML Element

  replaceWithTemplate replaces element with
  the HTML element referenced by url and passes
  referenced element to done.

  If an error occurs, replaceWithTemplate passes
  an Error object to done instead.
*/
function replaceWithTemplate (url, element, done) {
  if (!url) {
    var error = new Error ('[core][replaceWithTemplate] Error: an error occured while trying to load a template from "". A valid URL is needed.')
    strictError (error);
    done (error);
  }
  getTemplate (url,
    function (error, template) {
      if (error) { return done (error); }

      element.replaceWith (template);
      done (null, template);
  });
}

// Unittests for replaceWithTemplate.
unittest ('replaceWithTemplate',
  {elements: [$('<div><div class="test_block"></div></div>')]},
  function (assert, elements) {
    assert.expect (2);

    // I. Confirm that replaceWithTemplate can replace elements using templates.
    var done = assert.async ();
    replaceWithTemplate ('modules/example/templates/block.html', $('.test_block', elements [0]), function (error, template) {
      assert.notOk (error, 'replaceWithTemplate can replace an element using a template.');
      assert.strictEqual ($('h2', elements [0]).text (), 'Example Block', 'replaceWithTemplate inserted the correct template.');
      done ();
    });
});

/*
  getTemplate accepts three arguments:

  * url, a URL string
  * done, a function that accepts two arguments:
    an Error object and a JQuery HTML Element

  getTemplate loads the HTML template element
  referenced by url and passes it to done.

  If an error occurs, getTemplate throws a strict
  error and passes an error to done instead.
*/
function getTemplate (url, done) {
  $.get (url,
    function (html) {
      var template = $(html);
      done (null, template);
    },
    'html'
    ).fail (function () {
      var error = new Error ('[core][getTemplate] Error: an error occured while trying to load a template from "' + url + '".');
      strictError (error);
      done (error);
    });
}

/*
  getPlainText accepts three arguments:

  * url, a URL string
  * done, a function that accepts two arguments:
    an Error object and text

  getPlainText loads the plain text file
  referenced by url and passes it to done.

  If an error occurs, getPlainText throws a strict
  error and passes an error to done instead.
*/
function getPlainText (url, done) {
  $.get (url,
    function (content) {
      done (null, content);
    },
    'text'
    ).fail (function () {
      var error = new Error ('[core][getPlainText] Error: an error occured while trying to load a template from "' + url + '".');
      strictError (error);
      done (error);
    });
}

/*
  getPlainTextSafe accepts three arguments:

  * url, a URL string
  * done, a function that accepts two arguments:
    an Error object and text

  getPlainTextSafe loads the plain text file
  referenced by url and passes it to done.

  If an error occurs, getPlainTextSafe does not
  throw a strict error
*/
function getPlainTextSafe (url, done) {
  $.get (url,
    function (content) {
      done (null, content);
    },
    'text'
    ).fail (function () {
      var error = new Error ('[core][getPlainText] Error: an error occured while trying to load a template from "' + url + '".');
      console.log (error.message);
      done (error);
    });
}

/*
  strictError accepts one argument: error, an Error
  object. If the error mode has been set to strict,
  this function throws an exception with the given
  error. Note: the error mode is set by setting
  the "errorMode" parameter in settings.xml.
*/
function strictError (error) {
  if (STRICT_ERROR_MODE) {
    throw (error);
  } else {
    console.log (error.message);
  }
}

/*
  loadPage accepts three arguments:

  * id, a Page ID string

  loadPage triggers a Page Load Event using id
  as the page ID.
*/
function loadPage (id) {
  // I. Load the referenced page.
  // Note: The hashchange event handler is
  // responsible for actually loading the page
  // at this point.
  document.location.href = getContentURL (id);
}

/*
  getContentLink accepts two arguments:

  * id, a Resource ID String 
  * and label, an optional JQuery HTML Element.

  getContentLink returns a JQuery HTML Element
  that represents an HTML link to the resource
  referenced by id.

  getContentLink adds a click event handler to
  the link element that replaces Main Content
  element with the resource referenced by id.
*/
function getContentLink (id, label) {
  var link = $('<a></a>').attr ('href', getContentURL (id));
  return label ? link.html (label) : link;
}

/*
  getContentURL accepts a URI string, id, and
  returns a URL string that references the entry
  referenced by id.

  Note: Every valid id must be a URI string. The
  host must equal the name of the module that
  defined the content type and the first query
  parameter must equal the content type.
*/
function getContentURL (id) {
  return new URI ('').hash (id).toString ();
}

/*
  getIdFromURL accepts a Content URL as a URI and
  returns its ID parameter.
*/
function getIdFromURL (url) {
  return (url.fragment ().split ('#'))[0];
}

/*
  Accepts a Content URL as a URI object and
  returns the nested fragment identifier as
  a string.

  Note: ID's may contain nested fragment
  identifiers which can be used to create
  fragment links within pages.
*/
function getFragmentFromURL (url) {
  return (url.fragment ().split ('#'))[1];
}

/*
  getContentType accepts an Id string and returns
  the content type associated with the resource
  referenced by the id string.
*/
function getContentType (id) {
  var type = new URI (id).segmentCoded (0);
  // If the type parameter is invalid, throw a strict error
  if (!type) {
    strictError (new Error ('[core][getContentType] Error: "' + id + '" is an invalid id. The "type" path parameter is missing.'));
    return null;
  }
  return type;
}

/*
  getUniqueId returns an HTML id that is unique
  w.r.t the current document.
*/
function getUniqueId () {
  while ($('#id' + currentId).length > 0) {
    currentId ++;
  }
  return 'id' + (currentId ++);
}

/*
  Accepts one argument: variableName, a string
  that represents a variable name; and safely
  checks whether or not the referenced variable
  has been declared/defined.
*/
function declared (variableName) {
  return eval ('typeof ' + variableName + ' != "undefined"');
}

/*
  Accepts three arguments:

  * testName, a string that denotes a QUnit
    test name
  * environment, an Environment object
  * and test, a function that accepts two
    arguments: assert, a QUnit Assert object
    and elements, an array of JQuery
    HTML Elements

  safely creates the testing environment
  described by environment, executes test,
  and restores the original environment.

  environment is an object that has the
  following structure:

  {
    globals: [
      {variableName: VARIABLENAME,
       value: VALUE},
      ...
    ],
    elements: [JQUERYELEMENT, ...]
  }

  VARIABLENAME must be a string literal
  that represents the name of a global
  variable. VALUE, may be an arbitrary value
  expression. JQUERYELEMENT must be a JQuery
  HTML Element.

  unittest will set VARIABLENAME to VALUE
  during the test and restore VARIABLENAME to
  its original value afterwards.

  unittest will add JQUERYELEMENT to the DOM
  before executing the test and remove it
  afterwards.

  Example:

    unittest ('block_expandDocumentBlocks',
      {
        globals: [
          {variableName: 'variable',
           value: 12}
        ],
        elements: [$('<div></div>')]
      },
      function (elements, assert) {
        ...
    });
*/
function unittest (testName, environment, test) {

  // create abbreviation functions for QUnit.testStart and QUnit.testDone.
  function testStart (f) {
    QUnit.testStart (function (details) {
      details.name === testName && f ();
    });
  }
  function testDone (f) {
    QUnit.testDone (function (details) {
      details.name === testName && f ();
    });
  }

  // I. protect global variables.
  if (environment.globals) {
    environment.globals.forEach (function (entry) {
      // store the global variable's original value.
      var originalValue = eval (entry.variableName);

      // set the global variable's test value when the test begins.
      testStart (function () { eval (entry.variableName + ' = entry.value'); });

      // restore the global variable's original value when the test ends.
      testDone (function () { eval (entry.variableName + ' = originalValue'); });
    });
  }

  // II. create safe test elements.
  if (environment.elements) {
    // create a container element to store the test elements.
    var containerElement = $('<div></div>');

    // add the test element to the DOM when the test begins.
    testStart (function () { $('body').append (containerElement); });

    // remove the test element from the DOM when the test ends.
    testDone (function () { containerElement.remove (); });

    // add elements to the container element.
    environment.elements.forEach (function (element) {
      containerElement.append (element);
    });
  }

  // III. execute tests.
  QUnit.test (testName, function (assert) {
    test (assert, environment.elements);
  });
}
```

### Generating Source Files

You can generate the Core module's source files using [Literate Programming](https://github.com/jostylr/literate-programming), simply execute:
`literate-programming Readme.md`
from the command line.

#### Core.js
```
_"Core Module"

_"Global Variables"

_"The Module Load Handlers Store Class"

_"The Module Load Handlers Store"

_"The App Load Handlers Store Class"

_"The App Load Handlers Store"

_"The Load Event Handler"

_"Load Settings"

_"Load Modules"

_"Auxiliary Functions"
```
[core.js](#Core.js "save:")
