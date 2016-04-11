var sliderChanged = false;
var sliderValue = -1;
var currentTrial = 1;
var itemString = '<div align="center">\
        <div style="width:864px;">\
          <table border="0" style="width:100%">\
            <tr>\
              <td></td>\
              <td colspan="20">    \
                  <span id = "descriptionNUMBER"> </span>  \
              </td>\
            </tr>\
            <tr>\
              <td></td>\
              <td colspan="20" >    \
                  <span style="color:grey" id = "questionNUMBER"> </span>    \
              </td>\
            </tr>\
            <tr>\
              <td style="text-align:center"> <span id = "labelLowerNUMBER"></span>  </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="1" /></td>\
              <td> &nbsp; &nbsp; &nbsp; </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="2" /></td>\
              <td> &nbsp; &nbsp; &nbsp; </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="3" /></td>\
              <td> &nbsp; &nbsp; &nbsp; </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="4" /></td>\
              <td> &nbsp; &nbsp; &nbsp; </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="5" /></td>\
              <td> &nbsp; &nbsp; &nbsp; </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="6" /></td>\
              <td> &nbsp; &nbsp; &nbsp; </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="7" /></td>\
              <td> &nbsp; &nbsp; &nbsp; </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="8" /></td>\
              <td> &nbsp; &nbsp; &nbsp; </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="9" /></td>\
              <td> &nbsp; &nbsp; &nbsp; </td>\
              <td><input type="radio" name="ratingAnswerNUMBER" id = "radioAnswerNUMBER" value="10" /></td>\
              <td style="text-align:center"> <span id = "labelHigherNUMBER"></span> </td>\
            </tr>\
            </table>\
          </div>\
        </div>';
              
function getRandomInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
}

function add(a, b) {
    return a + b;
}

feetconversion = function (inches) {
  var mod = inches % 12;
  var feet = (inches - mod) / 12;
  var footunit, inchunit;
  if (mod == 1) {inchunit = ' inch'} else {inchunit = ' inches'};
  if (feet == 1) {footunit = ' foot '} else {footunit = ' feet '}; 
  return(feet + footunit + mod + inchunit);
}

function sliderChangeFunction(currentValue) {
  sliderChanged = true;
  sliderValue = currentValue;
}
  
function resetRadio() {
  var ele = document.getElementsByName("naturalAnswer");
  for(var i=0;i<ele.length;i++)
    ele[i].checked = false;
}



var RatingExperiment = function() {
  
  var count = 0;
  var expressionCount = 0;
  var nbins = 15;
  var trialData = [];
  var trialCount = 1;

  // we get variable *condition* from psiTurk
  var start;
  var quant = 'unspecified';
  var insertedSliders = false;
  var insertedRadios = false;
  // var allTrials = _.shuffle(setupTrials(items));
  var allTrials = _.shuffle(items);
  var expresssionShuffle = _.shuffle([0,1,2,3,6,7,8]);
  
  var next = function() {
    // if there are items left, start a new trial
    if (count < allTrials.length) {
      
      expresssionShuffle = _.shuffle([0,1,2,3,6,7,8]);
      
      resetRadio();
      
      var trial = allTrials[count];  
      currentTrial = trial;
      
      $('.description').hide();
      $('.slider-toclone').hide();
      //  dirty hack to make it look nicer
      $('#center').css('margin-right', 100);

      // setup the question, item & bin values for the next trial
      $('#question').text(trial.question);
      $('#scenario').html(['<b>',trial.scenario,'</b>'].join(''));
      
      
      // add rows with items
      $('#item1').html(itemString.replace(/NUMBER/g, "1"));
      $('#item2').html(itemString.replace(/NUMBER/g, "2"));
      $('#item3').html(itemString.replace(/NUMBER/g, "3"));
      $('#item4').html(itemString.replace(/NUMBER/g, "4"));
      $('#item5').html(itemString.replace(/NUMBER/g, "5"));
      $('#item6').html(itemString.replace(/NUMBER/g, "6"));
      $('#item7').html(itemString.replace(/NUMBER/g, "7"));
      
      
      // add descriptions
      $('#description1').html([trial.description,' ','<b>', trial.expression[expresssionShuffle[0]], '</b>.'].join(''));
      $('#description2').html([trial.description,' ','<b>', trial.expression[expresssionShuffle[1]], '</b>.'].join(''));
      $('#description3').html([trial.description,' ','<b>', trial.expression[expresssionShuffle[2]], '</b>.'].join(''));
      $('#description4').html([trial.description,' ','<b>', trial.expression[expresssionShuffle[3]], '</b>.'].join(''));
      $('#description5').html([trial.description,' ','<b>', trial.expression[expresssionShuffle[4]], '</b>.'].join(''));
      $('#description6').html([trial.description,' ','<b>', trial.expression[expresssionShuffle[5]], '</b>.'].join(''));
      $('#description7').html([trial.description,' ','<b>', trial.expression[expresssionShuffle[6]], '</b>.'].join(''));
      
      // add questions
      $('#question1').html(trial.question);
      $('#question2').html(trial.question);
      $('#question3').html(trial.question);
      $('#question4').html(trial.question);
      $('#question5').html(trial.question);
      $('#question6').html(trial.question);
      $('#question7').html(trial.question);
      
      
      // add slider labels
      $('#labelLower1').html(trial.labelLower);
      $('#labelLower2').html(trial.labelLower);
      $('#labelLower3').html(trial.labelLower);
      $('#labelLower4').html(trial.labelLower);
      $('#labelLower5').html(trial.labelLower);
      $('#labelLower6').html(trial.labelLower);
      $('#labelLower7').html(trial.labelLower);
      $('#labelHigher1').html(trial.labelHigher);
      $('#labelHigher2').html(trial.labelHigher);
      $('#labelHigher3').html(trial.labelHigher);
      $('#labelHigher4').html(trial.labelHigher);
      $('#labelHigher5').html(trial.labelHigher);
      $('#labelHigher6').html(trial.labelHigher);
      $('#labelHigher7').html(trial.labelHigher);
          
      // add the status report
      $('#statusBar').html([trialCount-1, " out of ", allTrials.length ,' complete'].join(''));
      
      // add condition, quantifier and item-topic to the trial data
      trialData.splice(0, 
                       0, 
                       trialCount, 
                       trial.antonymPair, 
                       trial.scenarioLabel, 
                       trial.predicateType,
                       trial.labelLower,
                       trial.labelHigher,
                       trial.expreLabel[expresssionShuffle[0]], 
                       trial.expression[expresssionShuffle[0]], 
                       trial.expreLabel[expresssionShuffle[1]], 
                       trial.expression[expresssionShuffle[1]], 
                       trial.expreLabel[expresssionShuffle[2]], 
                       trial.expression[expresssionShuffle[2]], 
                       trial.expreLabel[expresssionShuffle[3]], 
                       trial.expression[expresssionShuffle[3]], 
                       trial.expreLabel[expresssionShuffle[4]], 
                       trial.expression[expresssionShuffle[4]], 
                       trial.expreLabel[expresssionShuffle[5]], 
                       trial.expression[expresssionShuffle[5]], 
                       trial.expreLabel[expresssionShuffle[6]], 
                       trial.expression[expresssionShuffle[6]]
                       );
      
      start = + new Date();
      expressionCount = expressionCount + 1;
      trialCount = trialCount + 1;
      count = count + 1 ;

    }

    else {
      // end the experiment & show post-questionnaire
      new Questionnaire().start();
    }
  };

  var save = function(e) {
    e.preventDefault();
    var answer, mess, RT;

    RT = + new Date() - start;

    answer = _.map($('input[type="radio"]'), function(radio) {
      return $(radio).is(':checked') ? 1 : 0;
    });

    var radioValues = [$('input[name="ratingAnswer1"]:checked').val(),
                       $('input[name="ratingAnswer2"]:checked').val(),
                       $('input[name="ratingAnswer3"]:checked').val(),
                       $('input[name="ratingAnswer4"]:checked').val(),
                       $('input[name="ratingAnswer5"]:checked').val(),
                       $('input[name="ratingAnswer6"]:checked').val(),
                       $('input[name="ratingAnswer7"]:checked').val()];
    
        
    // if there is no checked element
    if (answer.reduce(add, 0) < 7) {
      mess = 'Please select an interpretation for each description.';
      alert(mess);
      return false;
    }

    // add the ratings to the trial data & save to server
    trialData = trialData.concat(radioValues[0],
                                 radioValues[1],
                                 radioValues[2], 
                                 radioValues[3], 
                                 radioValues[4], 
                                 radioValues[5], 
                                 radioValues[6], 
                                 RT);
    psiTurk.recordTrialData(trialData);

    console.log(trialData);
    
    trialData = []; // reset for next trial
    next();
  };

  psiTurk.showPage('item.html');
  $('#answered').on('click', save);
  next(); // start experiment
};
