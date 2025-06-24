{application, tic_tac_toe,
 [
  {description, "Tic Tac Toe Game Server"},
  {vsn, "1.0.0"},
  {modules, [
      tic_tac_toe_app,
      tic_tac_toe_sup,
      game_manager,
      game_session,
      websocket_handler,
      utils  % if you have this file, else remove from list
  ]},
  {registered, []},
  {mod, {tic_tac_toe_app, []}},
  {applications, [
      kernel,
      stdlib,
      cowboy,
      cowlib,
      jsx,
      jsone,
      uuid
  ]},
  {env, [
      {websocket_port, 8080}
  ]},
  {start_phases, []}
 ]}.
