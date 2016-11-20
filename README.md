# A Realm smart proxy for Foreman

[![Build Status](https://travis-ci.org/theforeman/foreman_ansible_inventory.svg?branch=master)](https://travis-ci.org/theforeman/foreman_ansible_inventory)

A simple smart proxy that creates LDAP entries on host creation and removes
them on destroy. It uses Foreman's realm smart proxy API.

## Configuration in Foreman

Add the smart proxy via the GUI. Foreman will detect the *realm* feature.

## Configuration of ldapsp
*ldapsp.config* has the ldap connection parameters while *policy.erl*
determines how the ldap entries will look like. The default policy creates
entries of the form:

    dn: cn=<hostname>, dc=example, dc=com
	objectclass: top
	objectclass: groupOfUniqueNames
    cn: <hostname>
    uniqueMember: cn=<hostname>, dc=example, dc=com

You can replace the module with whatever erlang module you like as long as
implements add_host/3 and del_host/2.

## Running
To run in development mode use

    make shell

To build releases, etc. check the [rebar3][] documentation.

[rebar3]: https://www.rebar3.org/v3.0/docs
