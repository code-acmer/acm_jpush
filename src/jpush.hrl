-ifndef(JPUSH_HRL).
-define(JPUSH_HRL, true).
-define(CONTENT_TYPE_JSON, "application/json;charset=utf-8").
-define(J_PLATFORM_IOS, <<"ios">>).
-define(J_PLATFORM_ANDROID, <<"android">>).
-define(J_IOS_DEFAULT, #jios{badge = <<"+1">>}).
-record(jplatform, {
         list = []
    }).

-record(jaudience, {
          tag = [],
          tag_and = [],
          alias = [],
          registration_id = []
    }).

-record(jnotification, {
          alert,
          jandroid,
          jios
    }).

-record(jandroid, {
         alert,
         title,
         builder_id,
         extras
      }).
-record(jios, {
         alert,
         sound,
         badge,
         extras
    }).
-endif.



