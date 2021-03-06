# Vagrant-based Setup for Snowdrift

## Install Vagrant

[Grab the latest Vagrant version](https://www.vagrantup.com/downloads.html)
for your system.

If you do not have a virtual machine program installed yet,
[install VirtualBox](https://www.virtualbox.org/wiki/Downloads).

## Install and run Snowdrift

    git clone https://git.gnu.io/snowdrift/snowdrift.git
    cd snowdrift
    vagrant up
    vagrant ssh
    cd /vagrant
    cabal install
    sdm init
    cabal install --enable-tests
    yesod devel

The site should now be running on <http://localhost:3000>.

Now you can play with Snowdrift locally.
To log into the site, use the built-in system with
user: `admin` pass: `admin`

To stop the site, hit the Enter key.

To then quit vagrant, run:

    exit
    vagrant halt

To run the site again later, open a terminal and then
**from your snowdrift directory** run:

    vagrant up
    vagrant ssh
    cd /vagrant
    yesod devel


## Workflow

Once going, `yesod devel` can stay running in one terminal while
you do work elsewhere.
It will rebuild and rerun the site whenever it detects file changes.

In cases where `yesod devel` fails to detect changes,
stop it with the Enter key, then run:

    cabal clean && yesod devel

If you add new dependencies (i.e. edit the `build-depends` field in
`Snowdrift.cabal`), you will need to run:

    cabal install

## More resources

See [BEGINNERS.md](BEGINNERS.md) for general info about contributing
and learning about the tools we use,
and see [GUIDE.md](GUIDE.md) for more technical details.
