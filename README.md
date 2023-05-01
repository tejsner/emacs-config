# emacs-config

## gpg

### import private key using paperkey

If public key is armored first run

```
gpg --dearmor public.key
```

Then run

```
paperkey --pubring public-key.gpg --secrets secret-key-paper.asc | gpg --import
```

## pass

### Backing up .password-store

Run the following command to encrypt and archive the password store.

```
tar -cz .password-store | gpg --sign --encrypt -r [key] > password-store-backup.tar.gz.gpg
```

Where `[key]` is your gpg key Restore the directory:

```
gpg --decrypt < password-store-backup.tar.gz.gpg | tar -xz 
```

