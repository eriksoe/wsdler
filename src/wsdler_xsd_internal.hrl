-ifndef(wsdler_xsd_internal_hrl_).
-define(wsdler_xsd_internal_hrl_, dummy).

-record(schema, {
          elements :: dict(), % of #element{}
          attributes :: dict(), % of ??
          groups :: dict(),   % of ??
          attr_groups :: dict(), % of ??
          types :: dict(),    % of typedef()
          type_order :: [qname()],
          targetNS :: string()
         }).

%%% State from phase 1:
-record(collect_state, {
          elements :: dict(), % of XML subtree
          attributes :: dict(), % of ??
          groups :: dict(),
          attr_groups :: dict(),
          types :: dict(),
          includes_etc :: [],
          targetNS :: string()
         }).

%%% State from phase 3:
-record(refcheck_state, {
          elements :: dict(), % of XML subtree
          attributes :: dict(), % of ??
          groups :: dict(),
          attr_groups :: dict(),
          types :: dict(),
          type_order :: [_],
          targetNS :: string()
         }).

-endif.
