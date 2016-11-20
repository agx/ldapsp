# A Realm smart proxy for the Foreman

[![Build Status](https://travis-ci.org/theforeman/foreman_ansible_inventory.svg?branch=master)](https://travis-ci.org/theforeman/foreman_ansible_inventory)

ldapsp is a simple smart proxy for the [Foreman][] that creates LDAP
entries on host creation and removes them on destroy. It uses
Foreman's realm [smart proxy API][].

## Running
To run in development mode use

    make shell

The proxy will then listen on port 8080. You can check this like

    $ curl http://localhost:8080/features
    ["realm"]

See *tests/test.sh* for more usage examples.  To build releases,
etc. check the [rebar3][] documentation.

## Configuration in the Foreman
Add the smart proxy via Foreman Web-GUI (Infrastructure → Smart
Proxies → New Smart Proxy). Foreman will detect the *realm* feature.

## Configuration of ldapsp
*priv/ldapsp.config* has the ldap connection parameters while
*priv/policy.erl* determines how the created ldap entries will look
like.  The default policy creates entries of the form:

    dn: cn=<hostname>, dc=example, dc=com
	objectclass: top
	objectclass: groupOfUniqueNames
    cn: <hostname>
    uniqueMember: cn=<hostname>, dc=example, dc=com

You can replace the module with whatever [Erlang][] module you like as
long as it implements *add_host/3* and *del_host/2*.

For SSL it's simplest to put e.g. apache in front of it.

[Foreman]: https://theforeman.org/
[rebar3]: https://www.rebar3.org/v3.0/docs
[smart proxy API]: http://projects.theforeman.org/projects/smart-proxy/wiki/API
[Erlang]: https://www.erlang.org/
