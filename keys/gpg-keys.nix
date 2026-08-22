{
  signingKey = "4968AA085CEBE8832D16B364AAE23FD4504DB519";

  # Both YubiKeys hold identical subkeys, so there is one keygrip,
  # one encryption subkey and one SSH key regardless of which card is inserted.
  sshKeygrips = [
    "B49B74B8CF7A9BE3A50807F80E2EE056239167E9" # [A] subkey 0xF3FE43769684A001
  ];

  encryptionSubkeys = [
    "F51D5E1AB9D5FCE2" # [E] subkey
  ];

  sshPublicKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ+98NazV+u5uJbw+jZrGK2s1RWBTNnqN/G+vxhqRBUa openpgp:0x9684A001"
  ];
}
