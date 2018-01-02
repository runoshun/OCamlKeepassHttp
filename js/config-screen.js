(function($) {
  $.fn.api.settings.api = {
    'get_state' : '/state',
    'save_state' : '/state',
    'post_action' : '/action',
    'delete_action' : '/delete_action',
    'restart_server' : '/restart_keepass_http',
  };
  $.fn.api.settings.successTest = function(response) {
      return !!(response) && !(response.error);
  };
  $.fn.api.settings.loadingDuration = 300;
  $(document).ready(function () {
    $('.ui.accordion').accordion();
    $('.menu .item').tab();

    var $server_status = $('#server-status');
    var $form = $('#config_form');
    var $error_msg = $('#error-msg');
    var $queued_actions = $('#queued-actions');
    var $associate_tmpl = $('#associate-tmpl');

    var updateServerRunningMsg = function(running) {
      if(running) {
        $server_status.removeClass("yellow");
        $server_status.text("Server Running");
      }
      else {
        $server_status.addClass("yellow");
        $server_status.text("Server Stopped");
      }
    };

    var updateError = function(error) {
      if(error) {
        $error_msg.removeClass('hidden');
        $error_msg.find('.content').text(error);
      }
      else {
        $error_msg.addClass('hidden');
      }
    };

    var updateScreen = function(response) {
      updateError(response.error);
      $.each(response.config, function(key, val) {
        if(val) {
          $('input[name="' + key + '"').val(val);
        }
      });

      updateActions(response.actions);
      updateServerRunningMsg(response.server_running);
    };

    var updateActions = function(actions) {
      if(actions && actions.length > 0) {
          var tmpls = [];
          for(var i = 0; i < actions.length; ++i) {
              var action = actions[i];
              switch (action.type) {
                case 'associate':
                    var $tmpl = $associate_tmpl.clone();
                    $tmpl.removeAttr('id');
                    $tmpl.find('.message .text').text("New Client Association Request");
                    $tmpl.find('.message .date').text(action.date);
                    $tmpl.find('.message .key').text('Client Key : ' + action.client);
                    $tmpl.find('.message .close').attr('data-id', action.id).on('click', function() {
                        var $self = $(this);
                        var data = JSON.stringify({ 'id' : $self.attr('data-id')});
                        $.ajax({
                          url : $.fn.api.settings.api['delete_action'],
                          type : 'POST',
                          data : data
                        }).done(function () {
                            $self.closest('.action').fadeOut('normal');
                        });
                    });
                    $tmpl.find('form input[name="id"]').val(action.id);
                    $tmpl.find('form').form({
                        inline : true,
                        fields : { 'data' : 'empty' }
                    });
                    $tmpl.find('form').api({
                        action : 'post_action',
                        method : 'POST',
                        beforeSend : function(settings) {
                            var arrayData = $tmpl.find('form').serializeArray();
                            var data = { };
                            $.map(arrayData, function(o,i) {
                              data[o['name']] = o['value']
                            });
                            settings.data = JSON.stringify(data);
                        },
                        onSuccess : function() {
                            $(this).closest('.action').fadeOut('normal');
                        }
                    });
                    tmpls.push($tmpl);
                    break;
                default :
                    break;
                }
          }
          $queued_actions.empty();
          for (var i in tmpls) {
              $queued_actions.append(tmpls[i]);
              tmpls[i].fadeIn('normal');
          }
          $queued_actions.show();
      }
      else {
          $queued_actions.hide();
      }
    };

    // initialize form
    $form.form({
        inline : true,
        fields: {
            keepass_db : 'empty',
        }
    });

    $form.api({
      action : 'get_state',
      on : 'now',
      onSuccess : updateScreen
    });

    $form.api({
      // custom request
      mockResponseAsync : function(settings, callback) {
        var arrayData = $form.serializeArray();
        var data = { };
        $.map(arrayData, function(o,i) {
          data[o['name']] = o['value']
        });
        settings.data = JSON.stringify(data);

        var saveState = function() {
          return $.ajax({
            url : settings.api['save_state'],
            type : 'POST',
            data : settings.data,
          });
        };
        var restartServer = function(data) {
          if(data.error) {
            var ret = new $.Deferred();
            ret.resolve(data);
            return ret;
          }
          else {
            return $.ajax({
              url : settings.api['restart_server'],
              type : 'POST',
              data : ''
            });
          }
        };

        var getState = function(prev_data) {
            var deferred = new $.Deferred();
            $.ajax({
                url : settings.api['get_state'],
                type : 'GET'
            }).done(function(data) {
                if(prev_data.error) {
                    data.error = prev_data.error;
                }
                deferred.resolve(data);
            }).fail(function(xhr,status,err) {
                deferred.resolve({ error : err });
            });
            return deferred;
        };

        var fail = function(xhr,status,err) {
          callback({error : err});
        };

        saveState()
            .then(restartServer,fail)
            .then(getState,fail)
            .then(callback,fail);
      },
      onSuccess : updateScreen ,
      onFailure : updateScreen
    });

    $form.find('[name="password"]').focus();

  });
})(jQuery);
