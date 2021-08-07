# DigiServices
DigiServices is a Cardano-based project that aims to be a trustworthy,
reward-driven, platform for goods and service exchanges

Table of Contents
* [Tokenomics](#tokenomics)
  * [Supply](#supply)
* [Alice and Bob example](#alice-and-bob-example)
* [White Paper](#white-paper)
  * [Abstract](#abstract)


Abstract

    1. Executive summary
    
    2. The  Challenge and the Solution
    
    3. DigiServices Platform for Trustworthiness
    
    4. Protocols Implementation
    
    5. Timeline
    

### Abstract
The growing need of goods and services exchange require a trustworthy social platform to act as escrow and eliminate non-delivery/non-payment crime, one of the largest and further growing one. Even though physical contracts appear to provide a solution, they are often misinterpreted because of their subjective language and they commonly lack the practicality needed in our digital word. We propose a platform that enables parties to create contracts stored in the Cardano blockchain and builds member trust based on token rewards and penalties, supported by a Credit Assessesment system covering every platform member. An algorithm collects statistics related to the service transactions, credit score, activity and returns the amount of tokens that will be rewarded. 
Furthermore parties that refuse to follow the rules pre-defined in the contract will be penalized by losing tokens proportionally to the severity of their misbehavior.
Digiservices strive for Trustworthiness implements policies to support just behavior and strongly penalize failures in fulfilling set agreements

### 1. Executive summary  - DigiServices Features
Digiservices strive for  Trustworthy Platform  excellence and for this purpose implements strict policies to support just behavior and strongly penalize failures in fulfilling set agreements.

# Intuitive
Create Smart Contracts in a few clicks, assisted also by an advanced interactive drag and drop editor
# Marketplace
Cut the cost: relying on available templates
# All In-Platform
Creating your smart legal contract, negotiate clauses with your client with an integrated dispute resolution system.
# High Customization
Ready made templates, clauses, and logical flows allow users to build flexible and versatile smart legal contracts for any need.

### 2. The  Challenge and the Solution
## The Problem - Non-Payment and Non-Delivery Crimes
The internet 2.0 revolution started end of the 90s solved a wide range of problems and vastly increased economic efficiency. But this revolution did very little to modernize our legal systems, which have not kept pace with technological and economic progress. Non-Payment and Non-Delivery Crimes show an exponential grow and accounted in Y2020 for 265 mio. USD losses in the US only (1).  In general cyber crimes are sky-rocketing to level still hard to foresee. 

## The Digiservices Solution
The solution proposed by Digiservices aims to overcome the limits of physical contracts.  Misinterpretation due to subjective language and the innate  lack of easy-handling make them useless to tackle the issue.
DigiServices is  a platform that enables parties to create contracts stored in the Cardano blockchain and builds members trust based on token rewards and penalties, supported by a Credit Assessment System (CAS) able to address every platform member. Additionally a reward-penalty system is  in place. An algorithm collects statistics related to the service transactions, credit score, activities and returns the amount of tokens that will be rewarded. 
Parties that refuse to follow the rules already pre-defined in the contract will be penalized by losing tokens proportionally to the severity of their misbehavior. In extreme cases access to the platform will be suspended.

# Governance
DigiServices is a platform that is developed with a community in mind, and as such it is only right that the community has a say over the direction of the platform. As such, Digiservices will implement a governance system using DSET tokens. These tokens will allow users of the platform to vote on proposed changes such as fee changes, safety protocol changes etc.

The token will have a total supply of 100 billion, the distribution of which will be as follows over a period of maximum 4 years:
 ● 50% - Members, over a period of 4 years
 ● 20% - Platform Development Fund 
● 10% - Core Team 
● 10% - Marketing Partner 
● 10% - Public Sale 

# Voting
Voting on the platform will utilize the quadratic voting method (2), whereby voting power of an individual user is proportional to the square root of their investment amount. This method, along with other precautions, prevent so-called 'whale' investors from gaining control of the voting process and denying other users the ability to cast votes.

# Utility token DSET
Blockchain can be used to create decentralized ecosystems in which a token is issued in order to fund and assist the development of an ecosystem and that can be later used to purchase goods /services or being an integral part of a service itself, benefiting from the advantages of a decentralized structure, eliminating or reducing the presence of a central intermediary body, thus
allowing the value shift from the center to the ends.
DigiServices’ vision encompasses this model where the blockchain is used to create a truly decentralized self-sustaining ecosystem. DigiServices strongly believes that the future of the internet lies in services powered by utility tokens, improving existing services with new paradigms that cannot be achieved in the absence of a distributed ledger.

# Nature and uses of the DSET Token
DSET token is a hybrid token that has both utility token characteristics and payment token characteristics. The DSET token is the token on which the ecosystem is based. 
Parties to a contractual relationship and members need DSET tokens for:
● Service transactions payments (depositing escrows(
● Trust Token Deposit
purchasing Smart Legal Contracts templates (both purchase fees paid to
creators and commissions on revenue paid by creators to Jur);
● Conflict fees payments
● Penalties payments
● Rewards accruals


## Business model
DigiServices believes that a decentralized economy must provide mechanisms for sustaining the platform that supports this trustworthy system

# Business Development Road Map
A basic one-time membership fee is requested to assure commitment and at the same time a membership low-barrier entry. Income generation is provided for the platform through transaction fees, for members acting as judge or as service provider being rewarded through spendable native token DSET .
Business development will occur at two levels:
1 - Development of own service/goods exchange platform with service offering and demand
2 - Affiliation with already existing or newly established service providers, focusing on enforcing unbiased Credit Assessment of all memberships accessing the platform

## Fees and Platform Income
DigiService’s choice is to adopt a flexible mechanism, which can be expanded or reduced according to the needs of the project and feedback from the members community.
DigiService therefore anticipates the use of fees and reserves the right to reduce or increase them on the basis of the progress of the project and the value of the DSET Token.
The following fees payable to DigiServices  apply at the moment:
● one-time membership fees
● accusation contract fees
● transaction fees for escrow 
● gas fees

## Dispute Event
Both parties have the possibility to open a dispute in the event  that the set contract is not executed fully by any of the two parties. The dispute is already integrated into the sealed smart-contract and the system is already set to smoothly settle any pending issues. For this purpose DSET token is deposited as Trust Token in a escrow deposit. A key feature is also that the Trust Token amount is suggested by the service provide and the client accepts it after negotiation, in the same way as service price bidding occurs.

### 3. DigiServices Platform for Trustworthiness

## Supply / Tokenomics
Differently from traditional currencies, such as Bitcoin, Litecoin and Ethereum, DigiServices tokens (DSET) are not deflationary. This is important in order to incentivise cooperative and honest behaviour in the platform.
A fixed amount of DSET tokens is monthly minted and distributed according to a Credit Assessment System (CAS). Users receive tokens proportionally to their scores, obeying the following function:

```haskell
-- The total amount of tokens that will be minted every month
totalAmt :: Int
totalAmt = 1000

-- Review is an integer between 0 and 100
calculateRewards :: [Review] -> [Int]
calculateRewards [] = []
calculateRewards (x:xs) = ((x `div` revSum) * totalAmt `div` 100) : calculateRewards xs
  where
    revSum :: Integer
    revSum = sum xs
```   
### Utility

In order to de-incentive dishonest behaviour, DigiServices makes it
possible for service providers to lock an arbitrary amount of tokens (Trust Token, TT) into
the contract proposal in the marketplace and request from the client the same amount. In
this way, there is commitment by both parties and a pledge in case of a conflict.

Thus DSET utility provides the basis for a trustworthy platform by setting a good measure of
reliability through the CAS (Credit Assessment System);furthermore it is an important element
for the decentralized voting mechanism about the platform development.

### Network

Users'activiy is rewarded: inviting new members, acting as a judge, scoring a high CAS provide
benefit for the member and for the platform as a whole. 

## Alice and Bob example

Suppose Alice wants to offer her services as a writer. In normal circumstances
she could search for a publishing company and sign a contract with them.
The problem, in this case, is that natural language contracts is open to
ambiguity and misinterpretation. Additionally, physical contracts draw-backs
are convenience and drafting management.

For Alice an alternative would be to access an online website focused on
freelancer jobs (e.g. Fiverr or Upwork). In this case, alongside with the
ambiguity and flexibility problems (as these websites usually make use of
pre-made natural language contracts), there could be the possibility of Alice
not delivering the project or even of the client not paying the agreed amount.

DigiServices tackles and solves these issues. A Cardano-smart-contract-based 
digital platform enables parties to offer their services avoiding misinterpratation
and ambiguity and leveraging a reputation system that penalizes dishonest parties
and reward honest ones.

Likewise freelancer online websites, through DigiServices Alice would be able to
access a user-friendly web application and publish her service. One of the key 
differences, though, will be the service not to be stored inside a centralized
database; the Datum of a Plutus Validator called 'marketplace' will store it.

The Datum of the marketplace contains a list of `Service`s. `Service` is a special
data type that holds four values: A `Title`, a `Description`, a `Price` and a
`Signature` symbol.

The first value (Title) is the service Alice will provide (e.g. "Novel writer").
The second value (Description) will expand on her background, including an extended 
explanation of what she will do. This content is qualitatively explained and will not
the judge decision in the event of a conflict. The third value (Price) is the amount of DSET tokens the client should pay in order
to receive the service.
The last value (Signature) is a non-fungible token NFT, valid only valid if it
contains a cryptographic signature resulted from the combination of the owner’s
private key and the `Accusation Contract` validator hash.

Example
```haskell
import Ledger

-- Defined explicitily for clarity
signAccusation :: Ledger.Crypto.PrivateKey -> Ledger.ValidatorHash -> Signature
signAccusation pk vh = Ledger.Crypto.sign vh pk

-- This will be stored in the Signature NFT metadata
data Sig = Sig { signatory          :: Ledger.PubKeyHash
               , accusationContract :: Ledger.ValidatorHash
               , signature          :: Signature
               } deriving (Eq, Show)
```

The signature proves that someone agreed with a determined contract.

In our example, Alice would first create an accusation contract. Let’s suppose
she uses Charlie, Daniel and Emma public keys as the list of judges.
Additionally, let’s say the inputs are `“Was a book actually written and
delivered?”`, `“Did it have more than 200 pages”` and `“Was the
client collaborative, providing any information needed?”`. Lastly, let’s
assume the written logic was the following:

```haskell
type ClientTokens = Int
type ProviderTokens = Int
type JudgeTokens = Int

type TTDistribution = (ClientTokens, ProviderTokens, JudgeTokens)
type TotalAmount = Int

distributeTokens :: Bool → Bool → Bool -> TotalAmount → Distribution
distributeTokens inp1 inp2 inp3 totalAmt =
    | (not inp1 || not inp2) && inp3 = ((totalAmt - judgeAmt), 0, judgeAmt)
    | inp1 && inp2 && not inp3 = (0, (totalAmt - judgeAmt), judgeAmt)
    | otherwise = (0, 0, judgeAmt)
  where
    judgeAmt :: Int
    judgeAmt = totalAmt `div` 20
```

Bob could agree with the contract and see that Alice's judges are
reliable and qualified and decide to actually request her services. For that
he would need to provide his signature token and lock the same amount of trust
tokens TT provided by Alice, as well as, the amount of DSET Alice set as her
service price. Let us assume Trust Token + Service fees = 60 DSET.

Supposing Alice is not meeting the agreed terms and decides to write a book with
only 100 pages (against the rules she herself defined and confirmed), Bob could
invoke an "Accuse" event inside the "accusation contract", which would notify the 
first judge in the list (Charlie) and give him a hardcoded fixed deadline (e.g. 24h)
to provide answers to the inputs defined by Alice ("Was a book actually...").

If he does, then the logic will be executed according to the inputs, provided
to each of the terms (e.g. `(True, False, True)`) and would distribute the tokens 
according to the agreed contract terms.
Because of the contract conditions, Alice would receive nothing, Bob 57 TT
and Charlie 3 TT. It is possible, though, that Charlie does not respond within
the deadline. In this case, the next judge in the list will be notified and the
cycle repeat.

Of course, in our example Bob was the one to invoke the accusation, but nothing
stops Alice to do the same in case Bob is not cooperative and does not follow
the agreed rules. DigiServices aims at preventive Non-Payment-Non-Delivery crimes,
assuring through contract conditions a deterministic result for unsatisfactory 
outcomes. This is achieved eliminating ambiguity normally attached to natural
language contracts and providing the ease of use extremely valubale and necessary
in our current society.

## Judges Selection Mechanism and Trustworthiness Assessment
Decentralized Selection is applied by DigiServices to select judges (arbitrators)
The community defines the objective requirements that the judges have to meet. Provided that judge candidate is already a platform member,  anyone interested in acting as a judge will apply for it and   stake a minimum set amount of DSET tokens. 

# Rewards and Penalty Systems
Digiservices strive for  Trustworthy Platform  excellence and for this purpose implements strict policies to support just behavior and strongly penalize failures in fulfilling set agreements.

-Rewards, DSET
    • Judges, when called in, will be rewarded for their service through a weighed formula including transaction  value percentage, accumulated experience measured as integrated into CAS score, staked tokens
    • Members having CAS scores exceeding a set threshold continuously over 12 months shall receive a premium-reward in DSET calculated over a formula weighing members activity-related factors
    • Aiming at increased participation and activity, members can freely set a  minimum transaction quantity threshold (no. of deals), measured per month. The higher the value set, the higher the premium-reward

-Penalties, P units 
    • Penalties, P units 
      Penalties will be accrued for:
    • -’guilty’ deal : 10x no of disputes lost in P units. This penalty adds up to lost Trust Token in case of negative dispute outcome. In case P units achieve a set maximum value during 12 months, the member is commanded to leave the platform and his assets are lost.
    • members receive yearly a set P goodwill amount, for covering up unexpected and unintentional ‘guilty’ disputes
      no. of deals per period: if it is below a pre-set threshold freely set by each member , a P units penalty is accrued 

### 4 . Protocols Implementation

## Review System
Each service provider is subjected to reviews in terms of score 1-10 by the client. The score received at every transaction determines an increase of  DSET into the Trust Fund allocated to each service provider. 0.45% of service fees are deposited into this fund and the cumulated value affects the CAS score of the service provider.
Additionally the Trust Fund serves to provide compensation in case of dispute loss by the service provider in addition to the already allocated Trust Token.
In case  dispute the client executes a redeemer associated with the service contract to have the full token amount paid back.
Protocol to be implemented

## CAS (Credit Assessment System) Calculation,  Protocol Implementation
Each platform member’s trustworthiness is assessed through a CAS score  to assess his/her trustworthiness. The CAS calculation formula relies on scores from transaction review system, Trust Fund value,  number of assessments, registration as judge, accumulated  transaction value over the period (3 months)

Protocol to be implemented

## Membership Rewards, Protocol Implementation

Period: 3 months

TxNind: Transaction no. over the period of the specific member

TxN : Total platform transactions no. over the period

CASind: individual CAS (range : 1 - 100)

StDSET: accumulated transaction value as service provider or client over the period 3 months  

fmDSET : set minimum amount of StDSET to get higher reward (freely set by each member)

AcJ: no. of calls  as judge over the period

Mrew: Membership  reward per period in DSET

If StDSET >= fm DSET then
Mrew = TxN * A  + 10, if Mrew < 5   # min reward is 5 DSET

where
	A = TxNind /TxN + CASind * 5/100 + 5* fmDSET +  B * 2 + AcJ * 5
	B= 1

If StDSET > fm DSET then
		B = StDSET/fmDSET * 10%


## Penalties,  Protocol Implementation

Penalties, P units 
      Penalties will be accrued for:
    • -’guilty’ deal : 10x no of disputes lost in P units. This penalty adds up to lost Trust Token in case of negative dispute outcome. In case P units achieve a set maximum value during 12 months, the member is commanded to leave the platform and his assets are lost.
    • members receive yearly a set P goodwill amount, for covering up unexpected and unintentional ‘guilty’ disputes
    • no. of deals per period: if it is below a pre-set threshold freely set by each member , a P units penalty is accrued 

Period: 3 months

TxDind: Transaction no. over the period causing disputes

TxDl : Transaction no. over the period causing lost disputes

CASind: individual CAS (range : 1 - 100)

StDSET: accumulated transaction value as service provider or client over the period 3 months  

fm DSET : set minimum amount of StDSET  get higher reward (freely set by each member)

MP: Membership  Penalties  in the set  period in P unit 


If StDSET >= fmDSET

	MP =  (10 * TxDl + TxDind * 0.5) * 60/CASind

If StDSET < fm DSET

	MP =  (10 * TxDl + TxDind * 0.5) * 60/CASind + 30 * fmDSET


## Judges Selection Mechanism, Protocol Implementation
to be completed



### Judges Inputs – AI-supported deterministic decisions → for MVP to be done ‘manually’
Judges are expected to provide ‘Inputs’ on occasion of ‘Conflict Event’.
Deterministic inputs are a pre-condition to assure a smooth and just conflict settlement. For this purpose trustworthy judges are required as well as pre-made contract clauses selected by DigiServices as those less prone to misunderstanding and misbehavior. The selection occurs through an integrated machine learning process trained through past transaction cases and which keep accumulating knowledge and thus reliability.

to be completed

## Judges rewards, Protocol Implementation
TxValue: Transaction Value

CASind: individual CAS (1 - 100 range)

StDSET: staked tokens average over 12 months  

fDSET : set minimum amount of StDSET to be staked to become judge

Jrew: Judge reward per transaction in DSET

If StDSET > fDSET then
	Jrew = TxValue * A + 10, if Jrew < 10   # min reward is 10 DSET

where 
A = 0.3% + (CASind * B%)/100 
B = 6 if StDSET = fDSET * 3
B = 3  if StDSET = fDSET * 3

Membership and Judge rewards are cumulative to encourage activity

## Conflict Event, Protocol Implementation
to be completed



### 5. Timeline
DigiServices aims to launch an early version of the platform in Dec. 2021. leveraging smart contracts availability on the Cardano blockchain mainnet. 


### Sources
(1) Statista 2021
(2) https://en.wikipedia.org/wiki/Quadratic_voting
