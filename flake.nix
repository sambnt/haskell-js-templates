{
  description = "Templates for new GHC Javascript backend";

  outputs = { self, nixpkgs, ... } @ inputs:
  {
    templates = {
      counter = {
        path = ./templates/01-counter;
        description = "Simple Haskell web template using the new GHC Javascript backend.";
      };

      auth-simple = {
        path = ./templates/02-auth;
        description = "Simple auth example using the new GHC Javascript backend.";
      };

      auth-oauth = {
        path = ./templates/03-auth-login;
        description = "Auth example with login using the new GHC Javascript backend.";
      };
    };

    defaultTemplate = self.templates.counter;
  };
}
