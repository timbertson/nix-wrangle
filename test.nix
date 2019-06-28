# TODO: why does this even succeed?
with (import <nixpkgs> {});
fetchFromGitHub {
	owner = "timbertson";
	repo = "gup";
	rev = "master";
	# sha256 = "1pwnmlq2pgkkln9sgz4wlb9dqlqw83bkf105qljnlvggc21zm3pv";
	sha256 = null;
}

