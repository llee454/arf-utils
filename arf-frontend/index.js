/*
  This module contains custom site-specific
  javascript that is called after all of the other
  block handlers have been executed.
*/

MODULE_LOAD_HANDLERS.add (
  function (done) {

  block_HANDLERS.addHandlers ({
    'main_prolog_query_block': main_prologQueryBlock,
    'main_record_meal_block': main_recordMealBlock
  });

  // I. Display/hide the Back to Top tab.
  setInterval (main_displayBackToTop, 1000);

  // II. Set the Back to Top tab's click event handler.
  $('#back_to_top').click (
    function (event) {
      event.preventDefault ();
      $('html, body').animate ({
        scrollTop: $('#top').offset ().top
      });
  });

  // III. control the mobile menu.
  $('#mobile-menu-header').click (function () {
    $('#mobile-menu-content').slideToggle ();
  })

  done (null);
});

// This function hides/displays the Back to Top tab.
function main_displayBackToTop () {
  if ($(window).scrollTop() > 200) {
    $('#back_to_top').animate ({opacity: 1});
  } else {
    $('#back_to_top').animate ({opacity: 0});
  }
}

function main_prologQueryBlock (context, done) {
  var inputElement = $('<input></input>')
    .attr ('id', 'prolog-query-input')
    .attr ('type', 'text');

  var responseElement = $('<div></div>').attr ('id', 'prolog-query-response');

  $(context.element)
    .append (inputElement)
    .append ($('<button></button>')
      .attr ('id', 'prolog-query-send')
      .text ('Send')
      .click (function () {
          var url = 'https://arf.larrylee.tech:5000/run?command=' + $('#prolog-query-input').val ()
          alert (url);
          $.get (url,
            function (content) {
              $('#prolog-query-response').text (content)
            }, 'text').fail (function () {
              alert ('failed');
            });
        }))
   .append ($('<div></div>')
     .text ('Response:')
     .append(responseElement));

  done (null);
}

function main_recordMealBlock (context, done) {
  var inputElement = $('<input></input>')
    .attr ('id', 'prolog-meal-calories-input')
    .attr ('type', 'text');

  var responseElement = $('<div></div>').attr ('id', 'prolog-meal-response');

  $(context.element)
    .append (inputElement)
    .append ($('<button></button>')
      .attr ('id', 'prolog-meal-send')
      .text ('Send')
      .click (function () {
          var url = 'https://arf.larrylee.tech:5000/run?command=nutrition:mealCreate(' + $('#prolog-meal-calories-input').val () + ', [serving(grain, 6)], _).';
          alert (url);
          $.get (url,
            function (content) {
              $('#prolog-meal-response').text (content)
            }, 'text').fail (function () {
              alert ('failed');
            });
        }))
   .append (responseElement);

  done (null);
}
