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
      <Pastel color=Red> "JoyBot Exchange Running on"</Pastel>
      " -> "
      <Pastel color=Yellow> (host ++ ":")</Pastel>
      <Pastel color=Yellow> (string_of_int(port)) </Pastel>
    </Pastel>
  );
let print_statement = (prefix, value) =>
  Pastel.(
    <Pastel color=WhiteBright>
      <Pastel color=Red> prefix </Pastel>
      " -> "
      <Pastel color=Yellow> value </Pastel>
    </Pastel>
  );
