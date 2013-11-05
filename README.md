DICT is a dictionary network protocol created by the DICT Development Group.[1] It is described by RFC 2229, published in 1997.

This application embeds a DICT client and allows you to query DICT servers via a simple RESTful JSON webservice:


A list of dictionaries available on the DICT server:

        /dictionaries


A list of matching strategies supported by the DICT server:

        /strategies


Find definitions of a word in all dictionaries:

        /define?w=WORD


Find definitions of a word in a certain dictionary:

        /define/DICTIONARY?w=WORD

For example:

        /define/muiswerk?w=meisje


Match a word using all strategies:

        /match?w=WORD

Match a word using a certain strategy:

        /match/STRATEGY?w=WORD


For example

        /match/prefix?w=appl


Match a word using a certain strategy in a certain dictionary:

        /match/STRATEGY/DICTIONARY?w=WORD


For example

        /match/prefix/magus?w=apple


2013 Yuri Leikind