# Plutus Environment

Differently from traditional applications, DigiServices does not make use of a
centralized server handling the application requests and storing the data inside
a database.

Instead, DigiServices takes advantage of Cardano's ecosystem and uses Plutus to
write smart contracts, which are able to handle requests in a decentralized way.
For that to work, there are two essential components: The "Marketplace" and the
"Accusation Contract".

## The Marketplace

The marketplace is a validator script responsible for handling service offers
and requests. This is made by storing a list of "Service" components inside the
Datum and locking an arbitrary amount of "Trust Tokens" (tokens used to indicate
someone's honesty).

This tokens stored in the marketplace can then, if needed, be consumed by an
"Accusation Contract" to ensure that dishonest peers are penalized and honest
ones rewarded.

### The Service

The "Service" is a data type that holds three values: The "Title", the
"Description" and the "Signature". The title and the description are used solely
for better explanation of what the service is about, it does not have an actual
effect on how the valdiator will be executed.

The signature, in the other hand, is essential for the functionality and
validity of a service. It is a non-fungible token that needs to be
"officialized" by an "Accusation Contract" and is used to prove compliance with
the contract terms. It can later be used against a user to claim someone's
disregard for the agreed rules.

A service can be offered in the marketplace by using an "Offer" redeemer and
providing a "Service" and can be requested by using a "Request" redeemer and
providing a "Signature".

### The Signature

The "Signature", as explained, is a NFT which holds, inside it's metadata, the
owner's public key hash, the "Accusation Contract" script address it refers to
and a cryptographic signature (with the script address as the signed message).

It is used for executing special actions in this contract, such as "Accuse" and
"Receive". These actions can only be done if the "Signature" was "officialized".
In other words, they can only be executed if this signature is registered inside
the accusation contract Datum.

## The Accusation Contract

The accusation contract is a Plutus validator script that can receive five
different types of redeemers: "Create", "Sign", "Accuse", "Judge" and "Receive".
It can also have different states (registered in the Datum): "Standard" and
"Waiting". "Accuse" can only be used in the "Standard" state, differently from
"Judge" and "Receive", which can only be used when the contract is in the
"Waiting" state.

### Create

Whenever someone want's to offer his service, he will need to create an accusation
contract. In order to do that the user must provide a list of "Judges" and a
list of "Inputs". The former is the public key hash of someone all parties trust.
The latter is a pair (String, Type), where the String is a question (e.g.
"Was the service provided?") and the Type is, well, the type (e.g. Bool).


### Sign

The sign redeemer officializes the "Signature" NFT by storing the token's symbol
inside the validator Datum so that the user (or an accuser) can, in the future,
prove someone actually agreed with the terms. It receives as an input the
signature token and immediatily returns it, as it is only used for proof.

### Accuse

The accuse redeemer can be used by any user that signed the contract to claim
justice. When used, the contract validator receives, as arguments, the
singnature and, if verified that the signature was officialized, changes the
contract state from "Standard" to "Waiting", notifying the responsible judges so
that he can provide answers to the "inputs".

### Judge

The judge redeemer can only be used by users that have their public key inside
the contract list of judges. When provided as a redeemer, the validator takes as
arguments the answers in the specified format and consumes the marketplace
validator script to distribute the locked trust tokens according to the logic
defined in the contract.

### Receive

After an accusation contract has been judged and the trust tokens distributed,
the parties can unlock their funds by using the receive redeemer.
