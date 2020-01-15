open Core;
open Console

let hello = () =>
  Pastel.(
    <Pastel color=WhiteBright>
      <Pastel color=Blue> "Congrats Dev team" </Pastel>
      ", "
      <Pastel color=Cyan> "It did not break" </Pastel>
      "!"
    </Pastel>
  );
let running = (~host="localhost", port) =>
  Pastel.(
    <Pastel color=WhiteBright>
      <Pastel color=Red> "PubSub server listening at"</Pastel>
      " -> "
      <Pastel color=Yellow> (host ++ ":")</Pastel>
      <Pastel color=Yellow> (string_of_int(port)) </Pastel>
    </Pastel>
  );
