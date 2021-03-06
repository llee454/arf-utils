/*
  The Form module provides a way to create and
  embed beautiful forms that use Google Forms
  to save and analyze responses
*/

/*
  A string constant that records a URL that
  points to this module's settings file.
*/
var form_SETTINGS_URL = 'modules/form/settings.xml';

/*
  A constant Form Settings object that represents
  this module's settings.

  Note: this object stores information about
  user defined form types.
*/
var form_SETTINGS = null;

/*
  A global flag that indicates that a honeypot
  element has detected bot ativity. When this
  variable is set to true, this module will not
  submit any forms.
*/
var form_BOT_DETECTED = false;

/*
  Loads the Google Form library and settings
  file before registering the Form block handler.
*/
MODULE_LOAD_HANDLERS.add (
  function (done) {
    loadScripts ([
        'modules/form/lib/google-form/google-form.js'
      ],
      function (error) {
        if (error) { return done (error); }

        form_loadSettings (form_SETTINGS_URL,
          function (error, settings) {
            if (error) { return done (error); }

            // Save the settings object.
            form_SETTINGS = settings;

            // Register the block handlers.
            block_HANDLERS.addHandlers ({
              'form_block': form_block,
              'form_radio_block': form_radioBlock,
              'form_honeypot_block': form_honeypotBlock
            });

            // Continue.
            done (null);
        });
    });
});

/*
  Accepts two arguments:

  * url, a URL string
  * and done, a function that accepts two
    arguments: error, an Error object; and
    settings, a Settings object

  loads the settings file referenced by url,
  parses the document, and passes the resulting
  settings object to done. If an error occurs,
  this function throws a strict error and passes
  the error to done instead.
*/
function form_loadSettings (url, done) {
  $.ajax (url, {
    dataType: 'xml',
    success: function (doc) {
      done (null, form_parseSettings (doc));
    },
    error: function (request, status, error) {
      var error = new Error ('[form][form_loadSettings] Error: an error occured while trying to load the Form module\'s settings.xml file from "' + url + '". ' + error);
      strictError (error);
      done (error);
    }
  });
}

/*
  Accepts one argument: doc, an XML Document
  that represents a settings file; parses the
  document; and returns the resulting settings
  object.
*/
function form_parseSettings (doc) {
  return {
    forms: $('settings > forms > form', doc)
           .map (function (i, formElement) {
             return form_parseForm ($(formElement));
           }).toArray ()
  };
}

/*
  Accepts an XML Element that represents a
  settings form entry, parses the element,
  and returns the resulting Form Type Entry
  object.
*/
function form_parseForm (formElement) {
  return {
    'name':      $('> name', formElement).text (),
    'url':       $('> url', formElement).text (),
    'template':  $('> template', formElement).text (),
    'blockBots': $('> blockBots', formElement).text () === 'true'
  };
}

/*
  Accepts two arguments:

  * context, a Block Expansion Context
  * and done, a function that accepts two
    arguments: error, an Error object; and element,
    a jQuery HTML Element that represents a form
    element.

  `context.element` must contain a single text
  node that represents a registered form type.

  This function creates a form of the given type,
  replaces `context.element` with the resulting
  form element, and passes the form element
  to done.

  If an error occurs, this function throws a
  strict error and passes the error to done
  instead.
*/
function form_block (context, done) {
  var formTypeName = context.element.text ().trim ();
  var formType = form_getFormType (formTypeName);
  if (!formType) {
    var error = new Error ('[form][form_block] Error: an error occured while trying to load a form. The "' + formTypeName + '" form has not been registered.');
    strictError (error);
    return done (error);
  }

  var form = new form_Form (formType);
  form.init (function (error) {
    if (error) { return done (error); }

    var featureElement = form.getFeatureElement ();
    context.element.replaceWith (featureElement);
    done (null, featureElement);
  });
}

/*
  Accepts two arguments: 

  * context, a Block Expansion Context
  * done, a function that accepts two arguments:
    an Error object and a JQuery HTML Element.

  context.element must be a DIV element that
  contains two child elements.

  The first must have a class named "entity_id"
  and contain a single text node representing
  the radio field's entity ID.

  The second must have a class named "options"
  and contain a single text node representing a
  JSON string.

  and replaces context.element with a radio
  field.

  Example:

  <div class="form_radio_block">
    <div class="entity_id">entry.566481996</div>
    <div class="options">[
      {
        "label": "The information is inaccurate.",
        "value": "The information is inaccurate."
      }
    ]</div>
  </div>
*/
function form_radioBlock (context, done) {
  getBlockArguments ([
      {'name': 'entity_id', 'text': true, 'required': true},
      {'name': 'options',   'text': true, 'required': true}
    ],
    context.element,
    function (error, blockArguments) {
      var options = JSON.parse (blockArguments.options.trim ());
      var element = form_createRadioFieldElement (
        blockArguments.entity_id.trim (),
        options
      );
      context.element.replaceWith (element);
      done (null, element);
  });
}

/*
  Accepts two arguments:

  * entityID, a string that represents a form
    element (entity) ID
  * options, an array of objects that have the
    following structure:
      {"label": LABEL, "value": VALUE}

  and returns an jQuery HTML element that
  represents a radio element.
*/
function form_createRadioFieldElement (entityID, options) {
  var name = getUniqueId ('form-radio-field-');
  var inputElement = $('<input></input>')
    .attr ('name', entityID)
    .attr ('type', 'hidden')
    .addClass ('form_radio_field_input');

  return $('<div></div>')
    .addClass ('form_radio_field')
    .append (inputElement)
    .append (options.map (function (option) {
      var id = getUniqueId ('form-radio-field-');

      var radioElement = $('<input></input>')
        .addClass ('form_radio_field_radio')
        .attr ('id', id)
        .attr ('type', 'radio')
        .attr ('name', name)
        .attr ('value', option ['value'])

      radioElement.change (function () {
        radioElement.prop ('checked') && inputElement.val (option ['value']);
      });

      var labelElement = $('<label></label>')
        .addClass ('form_radio_field_radio_label')
        .attr ('for', id)
        .text (option ['label']);

      return $('<div></div>')
        .addClass ('form_radio_field_container')
        .append (radioElement)
        .append (labelElement);
  }));
}

/*
  Accepts two arguments: 

  * context, a Block Expansion Context
  * done, a function that accepts two arguments:
    an Error object and a JQuery HTML Element.

  and replaces context.element with a form input
  element that when modified prevents this form
  from submitting.

  context.element must be a DIV element that
  contains a text node. This text will be
  used to name the element that the block will
  expand into.

  honeypot elements may be used to prevent
  bots from filling out forms created using
  this module.
*/
function form_honeypotBlock (context, done) {
  var element = $('<input></input>')
    .attr ('name', context.element.text ())
    .attr ('type', 'text')
    .addClass ('form_honeypot')
    .change (function () {
      form_BOT_DETECTED = true;
    });
  context.element.replaceWith (element);
  done (null);
}

/*
  Accepts one argument: formTypeName, a string
  that denotes a registered form type; and
  returns the form type entry that has the
  given name.
*/
function form_getFormType (formTypeName) {
  for (var i = 0; i < form_SETTINGS.forms.length; i ++) {
    var formType = form_SETTINGS.forms [i];
    if (formType.name === formTypeName) {
      return formType;
    }
  }
  return null;
}

/*
  Accepts a Form Type object and returns a
  Form object.
*/
function form_Form (type) {
  this._type           = type;
  this._index          = 0;
  this._num            = 0;
  this._featureElement = null;
}

/*
  Accepts no arguments and returns this form's
  Form Type object.
*/
form_Form.prototype.getType = function () { return this._type; }

/*
  Accepts no arguments and returns this form's
  index. Forms may have multiple sections. The
  index indicates which section the user is
  currently viewing.
*/
form_Form.prototype.getIndex = function () { return this._index; }

/*
  Accepts no arguments and return the number of
  sections that this form has.
*/
form_Form.prototype.getNum = function () { return this._num; }

/*
  Accepts no arguments and returns this form's
  feature element as a jQuery HTML Element.
*/
form_Form.prototype.getFeatureElement = function () { return this._featureElement; }

/*
*/
form_Form.prototype.getCurrentSectionElement = function () {
  return this.getSectionByIndex (this.getIndex ());
}

/*
  Accepts no arguments an initializes this form
  by creating a form feature element using the
  form type and setting certain properties.
*/
form_Form.prototype.init = function (done) {
  var self = this;
  getTemplate (this.getType ().template,
    function (error, featureElement) {
      if (error) { return done (error); }

      self._featureElement = featureElement;
      self._num = self.getNumSectionElements ();
      self.initFeatureElement ();

      done (null);
  });
}

/*
  Accepts no arguments and initializes this
  form's feature element.
*/
form_Form.prototype.initFeatureElement = function () {
  this.initFormElement ();
  this.initSectionElements ();
  this.initNextElements ();
  this.initPrevElements ();
  this.initLinkElements ();
  this.initButtonElements ();
  this.update ();
}

/*
  Accepts no arguments and initializes this
  form's form element.
*/
form_Form.prototype.initFormElement = function () {
  var self = this;
  this.getFormElement ()
    .attr ('action', this.getType ().url)
    .submit (function (event) {
        event.preventDefault ();
         self.shouldBlock () || window.submitGoogleForm (this);
      });
}

/*
*/
form_Form.prototype.initSectionElements = function () {
  this.getSectionElements ().each (function (index, sectionElement) {
    $(sectionElement).attr (form_getSectionIndexAttrib (), index);
  });
}

/*
  Accepts no arguments and initializes this
  form's next button elements.
*/
form_Form.prototype.initNextElements = function () {
  var self = this;
  this.getNextElements ().click (function () {
    self.next ();
  });
}

/*
  Accepts no arguments and initializes this
  form's previous button elements.
*/
form_Form.prototype.initPrevElements = function () {
  var self = this;
  this.getPrevElements ().click (function () {
    self.prev ();
  });
}

/*
*/
form_Form.prototype.initLinkElements = function () {
  var self = this;
  this.getLinkElements ().each (function (index, linkElement) {
    self.initLinkElement ($(linkElement));
  });
}

/*
*/
form_Form.prototype.initLinkElement = function (linkElement) {
  var self = this;
  var targetSectionId = form_getLinkTargetSectionAttrib (linkElement);
  linkElement.click (function () {
    self.goto (targetSectionId);
  });
}

/*
*/
form_Form.prototype.initButtonElements = function () {
  var self = this;
  this.getButtonElements ().each (function (index, buttonElement) {
    self.initButtonElement ($(buttonElement));
  });
}

/*
*/
form_Form.prototype.initButtonElement = function (buttonElement) {
  var self = this;
  buttonElement.click (
    function () {
      var targetEntryID = form_getButtonTargetEntryAttrib (buttonElement);
      if (targetEntryID) {
        var targetInputElement = self.getInputElementByEntryID (targetEntryID);
        targetInputElement.length > 0 &&
          targetInputElement.val (form_getButtonValueAttrib (buttonElement));
      }
      form_isSubmitButton (buttonElement) &&
        self.getSubmitButtonElements ().click ();
      var targetSectionID = form_getButtonTargetSectionAttrib (buttonElement);
      targetSectionID && self.goto (targetSectionID);
  });
}

/*
*/
form_Form.prototype.goto = function (targetSectionId) {
  var sectionElement = this.getSectionByID (targetSectionId);
  if (sectionElement.length === 0) { return; }

  var index = form_form_getSectionIndexAttrib (sectionElement);

  this._index = index;
  this.update ();
}

/*
  Accepts no arguments and moves the user to
  the next section.
*/
form_Form.prototype.next = function () {
  if (this.isEnd ()) { return; }

  this._index ++;
  this.update ();
}

/*
  Accepts no arguments and moves the user to
  the previous section.
*/
form_Form.prototype.prev = function () {
  if (this.isStart ()) { return; }

  this._index --;
  this.update ();
}

/*
  Accepts no arguments and updates the feature
  element classes and attributes to reflect the
  form's current state.
*/
form_Form.prototype.update = function () {
  this.setIndexAttrib ();

  var currentSectionElement = this.getCurrentSectionElement ();
  this.setActive (currentSectionElement);
}

/*
  Accepts no arguments and returns true iff
  this form should block the current submission.

  Note: forms should block submissions if they
  have been configured to block bot submissions
  and detect that the current submission is
  being generated by a bot.
*/
form_Form.prototype.shouldBlock = function () {
  return this.getType ().blockBots && form_BOT_DETECTED;
}

/*
  Accepts no arguments and returns true iff
  the user is currently on the last section of
  this form.
*/
form_Form.prototype.isEnd = function () {
  return this.getIndex () === this.getNum () - 1;
}

/*
  Accepts no arguments and returns true iff
  the user is currently on the first section of
  this form.
*/
form_Form.prototype.isStart = function () {
  return this.getIndex () === 0;
}

/*
  Accepts no arguments and updates the feature
  element's index attribute to represent the
  current state of this form.
*/
form_Form.prototype.setIndexAttrib = function () {
  this.getFeatureElement ().attr ('data-form-index', this.getIndex ());
}

/*
  Accepts no arguments and adds the "form_active"
  class to the currently active section of
  this form.
*/
form_Form.prototype.setActive = function (sectionElement) {
  var sectionElements = this.getSectionElements ();
  sectionElements.removeClass ('form_active');
  sectionElement.addClass ('form_active');
}

/*
*/
form_Form.prototype.getInputElementByEntryID = function (entryID) {
  return $('input[' + form_getEntryIDAttrib () + '="' + entryID + '"]', this.getFeatureElement ());
}

/*
*/
form_Form.prototype.getSectionByIndex = function (sectionIndex) {
  return $('[' + form_getSectionIndexAttrib () + '="' + sectionIndex + '"]', this.getFeatureElement ()); 
}

/*
*/
form_Form.prototype.getSectionByID = function (sectionId) {
  return $('[' + form_getSectionIDAttrib () + '="' + sectionId + '"]', this.getFeatureElement ());
}

/*
  Accepts no arguments and returns the number
  of section elements in this form's feature
  element.
*/
form_Form.prototype.getNumSectionElements = function () {
  return this.getSectionElements ().length;
}

/*
*/
function form_getLinkTargetSectionAttrib (linkElement) {
  return linkElement.attr (form_getTargetSectionAttrib ());
}

/*
*/
function form_form_getSectionIndexAttrib (sectionElement) {
  return sectionElement.attr (form_getSectionIndexAttrib ());
}

/*
*/
function form_getButtonTargetEntryAttrib (buttonElement) {
  return buttonElement.attr (form_getTargetEntryAttrib ());
}

/*
*/
function form_getButtonTargetSectionAttrib (buttonElement) {
  return buttonElement.attr (form_getTargetSectionAttrib ());
}

/*
*/
function form_getButtonValueAttrib (buttonElement) {
  return buttonElement.attr (form_getValueAttrib ());
}

/*
*/
function form_isSubmitButton (buttonElement) {
  return buttonElement.hasClass (form_getSubmitClass ());
}

/*
  Accepts no arguments and returns this form's
  form element as a jQuery HTML Element.
*/
form_Form.prototype.getFormElement = function () {
  return $('.form_form', this.getFeatureElement ());
}

/*
  Accepts no arguments and returns this form's
  section elements as a jQuery HTML Result Set.
*/
form_Form.prototype.getSectionElements = function () {
  return $('.form_form_section', this.getFeatureElement ());
}

/*
  Accepts no arguments and returns this form's
  next button elements as a jQuery HTML Result
  Set.
*/
form_Form.prototype.getNextElements = function () {
  return $('.form_next', this.getFeatureElement ());
}

/*
  Accepts no arguments and returns this form's
  previous button elements as a jQuery HTML
  Result Set.
*/
form_Form.prototype.getPrevElements = function () {
  return $('.form_prev', this.getFeatureElement ());
}

/*
*/
form_Form.prototype.getLinkElements = function () {
  return $('.form_link', this.getFeatureElement ());
}

/*
*/
form_Form.prototype.getButtonElements = function () {
  return $('.form_button', this.getFeatureElement ());
}

/*
  Note: there should only be one (possibly
  hidden) submit button. All other "submit"
  buttons have onclick event handlers that click
  the main button.
*/
form_Form.prototype.getSubmitButtonElements = function () {
  return $('[type="submit"]', this.getFeatureElement ());
}

/*
*/
function form_getValueAttrib () { return 'data-form-value'; }

/*
*/
function form_getSectionIndexAttrib () { return 'data-form-section-index'; }

/*
*/
function form_getSectionIDAttrib () { return 'data-form-section-id'; }

/*
*/
function form_getTargetSectionAttrib () { return 'data-form-target-section'; }

/*
*/
function form_getEntryIDAttrib () { return 'data-form-entry-id'; }

/*
*/
function form_getTargetEntryAttrib () { return 'data-form-target-entry'; }

/*
*/
function form_getSubmitClass () { return 'form_submit'; }
