-module(jpush_server).
-behaviour(gen_server).
-include("jpush.hrl").


-define(SERVER, ?MODULE).
-define(TIMEOUT,  30).
-record(state, {
          push_url,
          app_key,
          password,
          last_delay
    }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([broadcast_msg/1,
         delay_send_msg_to_alias/2,
         scheduled_send_msg_to_alias/3,
         send_msg_to_alias/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).


broadcast_msg(Text) ->
    gen_server:cast(?SERVER, {broadcast_msg, Text}).


scheduled_send_msg_to_alias([], _Text, When) ->
   ignore;
scheduled_send_msg_to_alias(Alias, Text, When) ->
    gen_server:cast(?SERVER, {scheduled_send_msg_to_alias, Alias, Text, When}).

send_msg_to_alias([], _Text) ->
   ignore;
send_msg_to_alias(Alias, Text) ->
    gen_server:cast(?SERVER, {send_msg_to_alias, Alias, Text}).

delay_send_msg_to_alias([], _Text) ->
    ignore;
delay_send_msg_to_alias(Alias, Text) ->
    gen_server:cast(?SERVER, {delay_send_msg_to_alias, Alias, Text}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Config]) ->
     %% io:format("~p~n",[Config]),
    State =
         #state{
            push_url = proplists:get_value(push_url, Config),
            app_key = proplists:get_value(app_key, Config),
            password = proplists:get_value(password, Config),
            last_delay = hmisctime:unixtime()
           },
    {ok, State, timeout_left(State)}.

handle_call(_Request, _From, State) ->
    {reply, ok, State, timeout_left(State)}.
handle_cast({scheduled_send_msg_to_alias, Alias, Text, When}, State) ->
   push(State,
        #jplatform{},
        #jaudience{alias = Alias},
        #jnotification{
            alert = Text,
            jios = ?J_IOS_DEFAULT
        }, When);
handle_cast({send_msg_to_alias, Alias, Text}, State) ->
   push(State,
        #jplatform{},
        #jaudience{alias = Alias},
        #jnotification{
            alert= Text,
            jios = ?J_IOS_DEFAULT

        }),
   {noreply, State, timeout_left(State)};

handle_cast({broadcast_msg, Text}, State) ->
    push(State,
         #jplatform{},
         #jaudience{},
         #jnotification{
            alert = Text,
            jios = ?J_IOS_DEFAULT
        }),
    {noreply, State, timeout_left(State)};

handle_cast({delay_send_msg_to_alias, Alias, Text}, State) ->
    add_delay(Text, Alias),
    {noreply, State, timeout_left(State)};

handle_cast(_Msg, State) ->
    {noreply, State, timeout_left(State)}.


handle_info(timeout, State) ->
    send_delay(State),
    NewState = State#state{last_delay = hmisctime:unixtime()},
    {noreply, NewState, timeout_left(NewState)};

handle_info(_Info, State) ->
    {noreply, State, timeout_left(State)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

push(State, Platform, Audience, Notification) ->
    push(State, Platform, Audience, Notification, undefined).

push(#state{
      push_url = PushUrl,
      app_key = AppKey,
      password = Password
    }, Platform, Audience, Notification, When) ->
    Body = json_encode(Platform, Audience, Notification),
    Base64 = base64:encode_to_string(AppKey ++ ":" ++ Password),
    Header = {"Authorization", "basic " ++ Base64},
    RequestUrl = get_request_url(PushUrl, When),

    io:format("request_url: ~p~n", [RequestUrl]),
    try
          case httpc:request(post,
                            {RequestUrl, [Header], ?CONTENT_TYPE_JSON, Body},
                            [],
                            [])  of
            {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                {ok, Body};
            {error, Reason} ->
                {error, Reason}
        end
    catch
       _:Any ->
          {error, Any}
    end.


get_request_url(PushUrl, undefined) ->
    PushUrl;
get_request_url(PushUrl, _When) ->
    PushUrl ++ "?delivery_type=scheduled&delivery_date=2015-06-09&delivery_time=18%3A46%3A22".


json_encode(Platform, Audience, Notification) ->
    Obj =
        {obj,
            [{platform, get_platform(Platform)},
             {audience, get_audience(Audience)},
             {notification, get_notification(Notification)},
             {options, {obj, [{time_to_live, 0}, {apns_production, true}]}}
            ]
        },
    JsonStr = rfc4627:encode(Obj),
    JsonStr.

get_platform(#jplatform{
                list = []
             }) ->
    <<"all">>;
get_platform(#jplatform{
                list = List
             }) ->
    List.

get_audience(#jaudience{
                tag = [],
                tag_and = [],
                alias = [],
                registration_id = []
             }) ->
    <<"all">>;
get_audience(#jaudience{
                tag = Tag,
                tag_and = TagAnd,
                alias = Alias,
                registration_id = RegistrationId
             }) ->
    List = [{tag, Tag},
            {tag_and, TagAnd},
            {alias, Alias},
            {registration_id, RegistrationId}],
    FilterList = filter_kv_list(List, []),
    {obj, FilterList}.





get_notification(#jnotification{
                    alert = Alert,
                    jandroid = Android,
                    jios = Ios
                }) ->
    {obj,
        [{alert, Alert},
         {android, get_android(Android)},
         {ios, get_ios(Ios)}]
    }.

get_android(undefined) ->
    {obj, []};
get_android(#jandroid{
               alert = Alert,
               title = Title,
               builder_id = BuilderId,
               extras = Extras
           }) ->
    List = [{alert, Alert},
            {title, Title},
            {builder_id, BuilderId},
            {extras, get_extras_obj(Extras)}],
    FilterList = filter_kv_list(List, undefined),
    {obj, FilterList}.

get_ios(undefined) ->
    {obj, []};
get_ios(#jios{
         alert = Alert,
         sound = Sound,
         badge = Badge,
         extras = Extras
        }) ->
    List = [{alert, Alert},
            {sound, Sound},
            {badge, Badge},
            {extras, get_extras_obj(Extras)}],
    FilterList = filter_kv_list(List, undefined),
    {obj, FilterList}.

filter_kv_list(List, NotValue) ->
    lists:filter(
        fun({_Key, Value}) ->
            if
                Value =/= NotValue ->
                    true;
                true ->
                    false
            end
        end, List).


get_extras_obj(undefined) ->
     undefined;
get_extras_obj(List) ->
    {obj, List}.


timeout_left(#state{
                last_delay = LastDelay
            }) ->
    Current = hmisctime:unixtime(),
    Seconds =
        if
            LastDelay + ?TIMEOUT < Current ->
                0;
            true ->
                ?TIMEOUT - (Current - LastDelay)
        end,
    Seconds * 1000.

add_delay(Text, Alias) ->
    Key = {delay, Text},
    case get(Key) of
        undefined ->
            put(Key, Alias);
        OldAlias ->
            put(Key, Alias ++ OldAlias)
    end.

send_delay(State) ->
    List = get(),
    lists:foreach(
        fun({{delay,Text} = Key, Alias}) ->
            erase(Key),
            % io:format("push ~ts to ~p~n", [Text, Alias]),
            push(State,
                 #jplatform{},
                 #jaudience{alias = Alias},
                 #jnotification{
                    alert= Text,
                    jios = ?J_IOS_DEFAULT

                 });
             (_) ->
                 ignore
        end, List).
