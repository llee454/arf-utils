/*
  This module contains custom site-specific
  javascript that is called after all of the other
  block handlers have been executed.
*/

MODULE_LOAD_HANDLERS.add (
  function (done) {

  block_HANDLERS.addHandlers ({
    'main_prolog_query_block': main_prologQueryBlock,
    'main_record_meal_block': main_recordMealBlock,
    'main_record_practice_session_block': main_recordPracticeSessionBlock
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
  var responseElement = $('<div></div>').attr ('id', 'prolog-meal-response');

  $(context.element)
    .addClass ('form')
    .append ($('<label></label>')
      .attr ('for', 'calories')
      .text ('Calories:'))
    .append ($('<input></input>')
      .attr ('id', 'prolog-meal-calories-input')
      .attr ('name', 'calories')
      .attr ('value', '0')
      .attr ('type', 'text'))

    .append ($('<label></label>')
      .attr ('for', 'grain')
      .text ('Grain servings:'))
    .append ($('<input></input>')
      .attr ('id', 'prolog-meal-grain-input')
      .attr ('name', 'grain')
      .attr ('value', '0')
      .attr ('type', 'text'))

    .append ($('<label></label>')
      .attr ('for', 'vegetables')
      .text ('Vegetable servings:'))
    .append ($('<input></input>')
      .attr ('id', 'prolog-meal-vegetables-input')
      .attr ('name', 'dairy')
      .attr ('value', '0')
      .attr ('type', 'text'))

    .append ($('<label></label>')
      .attr ('for', 'fruit')
      .text ('Fruit servings:'))
    .append ($('<input></input>')
      .attr ('id', 'prolog-meal-fruit-input')
      .attr ('name', 'fruit')
      .attr ('value', '0')
      .attr ('type', 'text'))

    .append ($('<label></label>')
      .attr ('for', 'dairy')
      .text ('Dairy servings:'))
    .append ($('<input></input>')
      .attr ('id', 'prolog-meal-dairy-input')
      .attr ('name', 'dairy')
      .attr ('value', '0')
      .attr ('type', 'text'))

    .append ($('<label></label>')
      .attr ('for', 'meat')
      .text ('Meat servings:'))
    .append ($('<input></input>')
      .attr ('id', 'prolog-meal-meat-input')
      .attr ('name', 'meat')
      .attr ('value', '0')
      .attr ('type', 'text'))

    .append ($('<label></label>')
      .attr ('for', 'nuts')
      .text ('Nuts servings:'))
    .append ($('<input></input>')
      .attr ('id', 'prolog-meal-nuts-input')
      .attr ('name', 'nuts')
      .attr ('value', '0')
      .attr ('type', 'text'))

    .append ($('<label></label>')
      .attr ('for', 'nuts')
      .text ('Sugar servings:'))
    .append ($('<input></input>')
      .attr ('id', 'prolog-meal-sugar-input')
      .attr ('name', 'sugar')
      .attr ('value', '0')
      .attr ('type', 'text'))

    .append ($('<button></button>')
      .attr ('id', 'prolog-meal-send')
      .text ('Send')
      .click (function () {
          var servingsString = [
              'prolog-meal-grain-input',
              'prolog-meal-vegetables-input',
              'prolog-meal-fruit-input',
              'prolog-meal-dairy-input',
              'prolog-meal-meat-input',
              'prolog-meal-nuts-input',
              'prolog-meal-sugar-input'
            ].map (function (id) {
              var value = $('#' + id).val ();
              var label = $('#' + id).attr ('name');
              return 'serving(' + label + ', ' + value + ')';
            }).join (', ');

          var url = 'https://arf.larrylee.tech:5000/run?command=nutrition:mealCreate(' + $('#prolog-meal-calories-input').val () + ', [' + servingsString + '], _).';
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

function main_recordPracticeSessionBlock (context, done) {
  var exercises = [
    {uuid: '94fd50ee-b04b-11ea-a138-0a29365a243a', name: "Almeida: Lesson 1"},
    {uuid: '9a84b0b6-b04b-11ea-9b9f-0a29365a243a', name: "Almeida: Lesson 2"},
    {uuid: 'a458b894-b04b-11ea-a197-0a29365a243a', name: "Almeida: Lesson 3"},
    {uuid: 'a458cd52-b04b-11ea-a99d-0a29365a243a', name: "Almeida: Lesson 4"},
    {uuid: 'a458de8c-b04b-11ea-bd9c-0a29365a243a', name: "Almeida: Lesson 5"},
    {uuid: 'a458ef62-b04b-11ea-9ac5-0a29365a243a', name: "Almeida: Lesson 6"},
    {uuid: 'a459002e-b04b-11ea-9756-0a29365a243a', name: "Almeida: Lesson 7"},
    {uuid: 'a4591122-b04b-11ea-9f2c-0a29365a243a', name: "Almeida: Lesson 8"},
    {uuid: 'a45921c6-b04b-11ea-b033-0a29365a243a', name: "Almeida: Lesson 9"},
    {uuid: 'a4593256-b04b-11ea-9eb9-0a29365a243a', name: "Almeida: Lesson 10"},
    {uuid: 'a4594408-b04b-11ea-895e-0a29365a243a', name: "Almeida: Lesson 11"},
    {uuid: 'a45960b4-b04b-11ea-8f38-0a29365a243a', name: "Almeida: Lesson 12"},
    {uuid: 'a4599ee4-b04b-11ea-9bda-0a29365a243a', name: "Almeida: Lesson 13"},
    {uuid: 'a459d828-b04b-11ea-9147-0a29365a243a', name: "Almeida: Lesson 14"},
    {uuid: 'a45a1310-b04b-11ea-88e4-0a29365a243a', name: "Almeida: Lesson 15"},
    {uuid: 'a45a4c36-b04b-11ea-b852-0a29365a243a', name: "Almeida: Lesson 16"},
    {uuid: 'a45a85ca-b04b-11ea-8550-0a29365a243a', name: "Almeida: Lesson 17"},
    {uuid: 'a45ac21a-b04b-11ea-a78a-0a29365a243a', name: "Almeida: Lesson 18"},
    {uuid: 'a45aff28-b04b-11ea-b759-0a29365a243a', name: "Almeida: Lesson 19"},
    {uuid: 'a45b3e66-b04b-11ea-8385-0a29365a243a', name: "Almeida: Lesson 20"},
    {uuid: 'a45b78d6-b04b-11ea-ad0a-0a29365a243a', name: "Almeida: Lesson 21"},
    {uuid: 'a45bb558-b04b-11ea-a12b-0a29365a243a', name: "Almeida: Lesson 22"},
    {uuid: 'a45bf2ca-b04b-11ea-83a6-0a29365a243a', name: "Almeida: Lesson 23"},
    {uuid: 'a45c2d08-b04b-11ea-b47b-0a29365a243a', name: "Almeida: Lesson 24"},
    {uuid: 'a4cea75c-b04b-11ea-b221-0a29365a243a', name: "Almeida: Lesson 25"},
    {uuid: '11b391f2-b04c-11ea-96fa-0a29365a243a', name: "Mangore: La Catedral 1"},
    {uuid: '13f9a79e-b04c-11ea-807a-0a29365a243a', name: "Mangore: La Catedral 3"}
  ];

  var responseElement = $('<div></div>').attr ('id', 'practice-response');

  var tableElement = $('<tbody></tbody>');

  $(context.element)
    .addClass ('form')
    .append ($('<label></label>')
      .attr ('for', 'duration')
      .text ('Duration (min):'))
    .append ($('<input></input>')
      .attr ('id', 'practice-duration-input')
      .attr ('name', 'duration')
      .attr ('type', 'text')
      .attr ('value', '0'))
    .append ($('<table></table>')
      .append (tableElement));

  exercises.forEach (exercise => {
    $(tableElement)
      .append ($('<tr></tr>')
        .append ($('<td></td>')
          .append ($('<input></input>')
            .attr ('name', exercise.uuid)
            .attr ('type', 'checkbox')))
        .append ($('<td></td>')
          .append ($('<label></label>')
            .attr ('for', exercise.uuid)
            .text (exercise.name))));
  });

  $(context.element)
    .append ($('<button></button>')
      .attr ('id', 'practice-send')
      .text ('Send')
      .click (function () {
         var exercisesStr = $(':checked', context.element)
           .map ((index, element) => '\'' + $(element).attr ('name') + '\'')
           .get ()
           .join (', ');

         var currRoutine = '351553e4-b03b-11ea-960c-0a29365a243a';
         var url = 'https://arf.larrylee.tech:5000/run?command=practice:sessionCreate(\'' + currRoutine + '\', ' + $('#practice-duration-input').val () + ', [' + exercisesStr + '], _).';
         alert (url);
         $.get (url,
           function (content) {
             $('#practice-response').text (content)
           }, 'text').fail (function () {
             alert ('failed');
           });
       }))
    .append (responseElement);

  done (null);
}
