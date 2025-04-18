{
  epkgs,
  version
}:

epkgs.elpaBuild {
    pname = "gh-autolinks";
    ename = "gh-autolinks";
    version = version;
    src = [ ./gh-autolinks.el ];
    packageRequires = [];
    meta = {};
}
