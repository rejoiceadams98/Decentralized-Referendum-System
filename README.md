# 🗳️ Decentralized Referendum System

A decentralized voting system built on Stacks blockchain using Clarity smart contracts.

## 🎯 Features

- Create referendum proposals
- Vote on active proposals
- Automatic proposal finalization
- Minimum vote threshold
- Administrative controls

## 🔧 Contract Functions

### For Everyone
- `create-proposal`: Create a new referendum proposal
- `vote`: Cast a vote on an active proposal
- `get-proposal`: View details of a specific proposal
- `get-vote`: Check if and how someone voted
- `get-proposal-count`: Get total number of proposals
- `finalize-proposal`: Complete a proposal after end time

### Admin Only
- `set-admin`: Transfer admin rights
- `set-min-votes`: Adjust minimum vote threshold

## 📝 Usage Example

1. Create a proposal:
```clarity
(contract-call? .referendum create-proposal "New Park" "Should we build a new park?" u1000)
```

2. Vote on a proposal:
```clarity
(contract-call? .referendum vote u1 true)
```

3. Finalize after end block:
```clarity
(contract-call? .referendum finalize-proposal u1)
```

## ⚙️ Setup

1. Install Clarinet
2. Create new project
3. Copy contract to `contracts/referendum.clar`
4. Deploy and test

## 🔒 Security

- One vote per address
- Time-locked voting periods
- Admin-controlled minimum vote threshold


