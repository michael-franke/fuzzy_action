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
  var expresssionShuffle = _.shuffle([0,1,2,3,4,5,6]);
  
  var next = function() {
    // if there are items left, start a new trial
    if (count < allTrials.length) {
      
      var trial = allTrials[count];
      

      if (!insertedRadios) {
      insertRadio();
      insertedRadios = true;
      }

      $('#radios').show();
      resetRadio();
      $('.description').hide();
      $('.slider-toclone').hide();
      //  dirty hack to make it look nicer
      $('#center').css('margin-right', 100);

      // setup the question, item & bin values for the next trial
      $('#question').text(trial.question);
      $('#scenario').html(trial.scenario);
      $('#description').html(['<b>', trial.description,, ' ', trial.expression[expresssionShuffle[expressionCount]], '.</b>'].join(''));

      // add condition, quantifier and item-topic to the trial data
      trialData.splice(0, 0, trialCount, trial.antonymPair, trial.scenarioLabel, trial.expreLabel[expresssionShuffle[expressionCount]], trial.expression[expresssionShuffle[expressionCount]]);

      // add the bin specific text under each slider
      _.each(trial.binLabels, function(bin, i) {
        $('#bin' + i).text(bin);
      });

      start = + new Date();
      expressionCount = expressionCount + 1;
      trialCount = trialCount + 1;
      if (expressionCount == 7) {
        expressionCount = 0 ;
        count = count + 1 ;
        expresssionShuffle = _.shuffle([0,1,2,3,4,5,6]);
      }
        
          
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

    // if there is no checked element
    if (!_.any(answer)) {
      mess = 'Please click on one interval.';
      alert(mess);
      return false;
    }

    var bin = answer.indexOf(1);

    // add the ratings to the trial data & save to server
    trialData = trialData.concat(bin, RT);
    psiTurk.recordTrialData(trialData);

    trialData = []; // reset for next trial
    next();
  };

  psiTurk.showPage('item.html');
  $('#answered').on('click', save);
  next(); // start experiment
};
