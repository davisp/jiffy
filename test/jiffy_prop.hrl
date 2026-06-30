% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.
%

% Use from -ifdef(WITH_PROPER) sections.

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

% Auto-discover every prop_*/0 function exported by the including module. Idea copied from CouchDB.
% The ['prop_' .. 'prop`'] construct picks atoms beginning with "prop_"
-define(JIFFY_QUICKCHECK(Timeout, NumTests),
    [
        {atom_to_list(F_),
            {timeout, Timeout,
                ?_assert(proper:quickcheck(?MODULE:F_(),
                    [{numtests, NumTests}, {to_file, user}]))}}
        || {F_, 0} <- ?MODULE:module_info(exports),
           F_ > 'prop_', F_ < 'prop`'
    ]).

-define(JIFFY_QUICKCHECK(Timeout), ?JIFFY_QUICKCHECK(Timeout, 200)).
