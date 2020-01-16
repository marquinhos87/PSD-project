%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.8.0

-ifndef(nefitproto).
-define(nefitproto, true).

-define(nefitproto_gpb_version, "4.8.0").

-ifndef('MSGAUTH_PB_H').
-define('MSGAUTH_PB_H', true).
-record('MsgAuth',
        {name                   :: iodata(),        % = 1
         pass                   :: iodata(),        % = 2
         ctype                  :: 'MANUFACTURER' | 'IMPORTER' | integer(), % = 3, enum MsgAuth.ClientType
         mtype = 'LOGIN'        :: 'LOGIN' | 'REGISTER' | integer() | undefined % = 4, enum MsgAuth.MsgType
        }).
-endif.

-ifndef('MSGACK_PB_H').
-define('MSGACK_PB_H', true).
-record('MsgAck',
        {ok                     :: boolean() | 0 | 1, % = 1
         msg                    :: iodata() | undefined % = 2
        }).
-endif.

-ifndef('DISPONIBILITYS_PB_H').
-define('DISPONIBILITYS_PB_H', true).
-record('DisponibilityS',
        {nameM                  :: iodata(),        % = 1
         nameP                  :: iodata(),        % = 2
         minimun                :: integer(),       % = 3, 32 bits
         maximun                :: integer(),       % = 4, 32 bits
         value                  :: integer(),       % = 5, 32 bits
         period                 :: integer()        % = 6, 32 bits
        }).
-endif.

-ifndef('DISPONIBILITYN_PB_H').
-define('DISPONIBILITYN_PB_H', true).
-record('DisponibilityN',
        {nameM                  :: iodata(),        % = 1
         nameP                  :: iodata(),        % = 2
         minimun                :: integer(),       % = 3, 32 bits
         maximun                :: integer(),       % = 4, 32 bits
         value                  :: integer(),       % = 5, 32 bits
         period                 :: integer()        % = 6, 32 bits
        }).
-endif.

-ifndef('ORDERS_PB_H').
-define('ORDERS_PB_H', true).
-record('OrderS',
        {nameM                  :: iodata(),        % = 1
         nameP                  :: iodata(),        % = 2
         quant                  :: integer(),       % = 3, 32 bits
         value                  :: integer()        % = 4, 32 bits
        }).
-endif.

-ifndef('ORDERN_PB_H').
-define('ORDERN_PB_H', true).
-record('OrderN',
        {nameM                  :: iodata(),        % = 1
         nameP                  :: iodata(),        % = 2
         quant                  :: integer(),       % = 3, 32 bits
         value                  :: integer()        % = 4, 32 bits
        }).
-endif.

-ifndef('ORDERACKS_PB_H').
-define('ORDERACKS_PB_H', true).
-record('OrderAckS',
        {ack                    :: boolean() | 0 | 1, % = 1
         msg                    :: iodata() | undefined, % = 2
         nameI                  :: iodata()         % = 3
        }).
-endif.

-ifndef('ORDERACKI_PB_H').
-define('ORDERACKI_PB_H', true).
-record('OrderAckI',
        {ack                    :: boolean() | 0 | 1, % = 1
         msg                    :: iodata() | undefined % = 2
        }).
-endif.

-ifndef('SUBS_PB_H').
-define('SUBS_PB_H', true).
-record('SubS',
        {subs = []              :: [iodata()] | undefined % = 1
        }).
-endif.

-ifndef('SUBN_PB_H').
-define('SUBN_PB_H', true).
-record('SubN',
        {subs = []              :: [iodata()] | undefined % = 1
        }).
-endif.

-ifndef('PRODUCTIONS_PB_H').
-define('PRODUCTIONS_PB_H', true).
-record('ProductionS',
        {nameM                  :: iodata(),        % = 1
         nameP                  :: iodata(),        % = 2
         quant                  :: integer(),       % = 3, 32 bits
         value                  :: integer()        % = 4, 32 bits
        }).
-endif.

-ifndef('PRODUCTIONM_PB_H').
-define('PRODUCTIONM_PB_H', true).
-record('ProductionM',
        {nameP                  :: iodata(),        % = 1
         quant                  :: integer(),       % = 2, 32 bits
         value                  :: integer()        % = 3, 32 bits
        }).
-endif.

-ifndef('RESULTS_PB_H').
-define('RESULTS_PB_H', true).
-record('ResultS',
        {result                 :: boolean() | 0 | 1, % = 1
         msg                    :: iodata() | undefined, % = 2
         nameI                  :: iodata()         % = 3
        }).
-endif.

-ifndef('RESULTI_PB_H').
-define('RESULTI_PB_H', true).
-record('ResultI',
        {result                 :: boolean() | 0 | 1, % = 1
         msg                    :: iodata() | undefined % = 2
        }).
-endif.

-ifndef('INFOS_PB_H').
-define('INFOS_PB_H', true).
-record('InfoS',
        {nameM                  :: iodata(),        % = 1
         nameP                  :: iodata(),        % = 2
         minimun                :: integer(),       % = 3, 32 bits
         maximun                :: integer(),       % = 4, 32 bits
         value                  :: integer(),       % = 5, 32 bits
         period                 :: integer(),       % = 6, 32 bits
         nameI                  :: iodata()         % = 7
        }).
-endif.

-ifndef('INFOI_PB_H').
-define('INFOI_PB_H', true).
-record('InfoI',
        {nameM                  :: iodata(),        % = 1
         nameP                  :: iodata(),        % = 2
         minimun                :: integer(),       % = 3, 32 bits
         maximun                :: integer(),       % = 4, 32 bits
         value                  :: integer(),       % = 5, 32 bits
         period                 :: integer()        % = 6, 32 bits
        }).
-endif.

-ifndef('SERVER_PB_H').
-define('SERVER_PB_H', true).
-record('Server',
        {msg                    :: {m1, nefitproto:'DisponibilityS'()} | {m2, nefitproto:'OrderS'()} | {m3, nefitproto:'SubS'()} | {m4, nefitproto:'ResultS'()} | {m5, nefitproto:'InfoS'()} | {m6, nefitproto:'ProductionS'()} | {m7, nefitproto:'OrderAckS'()} | undefined % oneof
        }).
-endif.

-ifndef('GETS_PB_H').
-define('GETS_PB_H', true).
-record('GetS',
        {nameI                  :: iodata()         % = 1
        }).
-endif.

-ifndef('GETN_PB_H').
-define('GETN_PB_H', true).
-record('GetN',
        {nameI                  :: iodata()         % = 1
        }).
-endif.

-ifndef('NEGOTIATIONSS_PB_H').
-define('NEGOTIATIONSS_PB_H', true).
-record('NegotiationsS',
        {nameI                  :: iodata(),        % = 1
         negotiations = []      :: [nefitproto:'InfoI'()] | undefined % = 2
        }).
-endif.

-ifndef('NEGOTIATIONSI_PB_H').
-define('NEGOTIATIONSI_PB_H', true).
-record('NegotiationsI',
        {negotiations = []      :: [nefitproto:'InfoI'()] | undefined % = 1
        }).
-endif.

-endif.
