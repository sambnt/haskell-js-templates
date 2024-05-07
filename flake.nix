{
  description = "Templates for new GHC Javascript backend";

  outputs = { self, nixpkgs, ... } @ inputs:
  {
    templates = {
      cross = {
        path = ./templates/01-counter;
        description = "Simple Haskell web template using the new GHC Javascript backend.";
      };
    };
  };
}
