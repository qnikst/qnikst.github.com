----
title: Making personal environment and long live dotfiles
author: Alexander Vershilov
date: 2018-08-22
tags: nix, configuration
----

Some time ago I installed [NixOS], and one thing that I had a problem with
was understanding how to set up user environment. You can can configure NixOS
declaratively with a description in a single file `/etc/nixos/configuration.nix`, but
all applications set up there get installed system-wide. Instead, you
usually want to have your user environment to be configured per your user.
Also you may want to move your environment from one host to another (possibly
to another distribution or even to another OS). Updating configuration.nix
looks like a too heavy-weight solution for such a task.

Other solutions that I've seen were building of one's own environment or using
[nix-env][nix-env-tldr] for installing applications per user. I was not able to
adopt the former solution, and the latter does not provide declarative config,
and one may collect lots of garbage of applications she runs once. Besides, it
would be nice to control dotfiles in the same style.

One day my colleague [Nicolas Mattia][nasm] suggested me to look at the tool he
uses, called [homies], the one he had excellently described in his [blog][nasm-blog]. I
think everyone should read and check that :). Unfortunately, this approach, at least
up to my understanding should exist for each user, because it's not customisable.
So I've started my project [homster] that is solving the same problem. Currently
it's primarily based on the `homies` though in the future it's going to diverge.

The general structure of the project is as follows. There is a `default.nix` file
that describes all the packages and the modified ones that I use. In each modified
package I pass my config, or it's configured to read system config from
the nix package directory so I can configure common options in the homster project
and update them on the host by usual means.

So it seems that I have found the solution to all my needs and I can have a declarative
configuration for the user environment. I still can use `nix-env -i` for temporarily
needed packages, but `nix-env -f homster/default.nix -i --remove-all` command updates
and cleans my environment. Most of the dotfiles can be kept in the project.
Also, I can set up my environment anywhere where I can install `nix` package
manager.

While implementing this the most interesting problem was `git`. Git searches for its
configuration in 3 places according to the man page:

  1. system-wide: `(prefix)/gitconfig`
  2. user-wide: `$(HOME)/.gitconfig` or `$XDG_CONFIG_DIR/git/gitconfig`
  3. project-wide: `~/.git/config`

Besides, it takes `--config` option that overrides config search entirely. We
can't use `--config` because that overrides project specific options, we can't
update user-wide file with nix, so the only option left is the system-wide config.
So we need to understand what is `(prefix)`. Man pages tell that it's a value of
the `PREFIX` environment option. Let's check nixpkgs:

```nix
    "prefix=\${out}"
```

https://github.com/NixOS/nixpkgs/blob/54ba2c9afca07b0f14763b3697d00b637b2461e0/pkgs/applications/version-management/git-and-tools/git/default.nix#L86

It seems that nix installs that, but let us check it:

```bash
strace git config 2>&1 | grep gitconfig
access("/etc//gitconfig", R_OK)         = -1 ENOENT (No such file or directory)
```

Seems not to be what we were expecting. The story continues, we need to understand where
does git looks for a config. You can find it on GitHub (modulo the version
that I've used, but it's irrelevant for the code I'm interested in)
<https://github.com/git/git/blob/7e8bfb0412581daf8f3c89909f1d37844e8610dd/config.c#L1634-L1640>

```c
const char *git_etc_gitconfig(void)
{
	static const char *system_wide;
	if (!system_wide)
		system_wide = system_path(ETC_GITCONFIG);
	return system_wide;
}
```

Okay, what is `ETC_GITCONFIG` <https://github.com/git/git/blob/1f1cddd558b54bb0ce19c8ace353fd07b758510d/configure.ac#L387-L391>

```bash
GIT_PARSE_WITH_SET_MAKE_VAR(gitconfig, ETC_GITCONFIG,
			Use VALUE instead of /etc/gitconfig as the
			global git configuration file.
			If VALUE is not fully qualified it will be interpreted
			as a path relative to the computed prefix at runtime.)
```

Finally, after searching what `GIT_PARSE_WITH_SET_MAKE_VAR` means, we can find that
we need to pass `--with-gitconfig=name` as a parameter to the configure.

So now we need to patch nix package for `git` to do that. As usual, we want
to make it declaratively and easy to change without redoing the work that NixOS maintainers
already did like applying patches and packaging.

Everything described above can be done pretty quickly in `nix`, in my `homster/git/default.nix`
I can override default package for git as:

```nix
git.overrideAttrs (old: {
   configureFlags = [ "--with-gitconfig=$out/etc/gitconfig ];
})
```

`nix` interprets the `$out` variable and substitutes exact hash there. Then we can
copy our file to the right place and get a new system-wide configuration that is
controlled by us.

```bash
strace git config 2>&1 | grep gitconfig
access("/nix/store/66hi8rssnvhlxbwjg3qkc4bcs76fp8np-git-2.16.4/etc/gitconfig", R_OK) = 0
openat(AT_FDCWD, "/nix/store/66hi8rssnvhlxbwjg3qkc4bcs76fp8np-git-2.16.4/etc/gitconfig", O_RDONLY) = 3
```

exactly what is needed.


[NixOS]: https://nixos.org/
[nix-env-tldr]: https://github.com/tldr-pages/tldr/blob/master/pages/common/nix-env.md
[nasm]: https://github.com/nmattia
[homies]: https://github.com/nmattia/homies
[nasm-blog]: http://nmattia.com/posts/2018-03-21-nix-reproducible-setup-linux-macos.html
[homster]: https://github.com/qnikst/homster

