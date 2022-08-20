{}:

{
  mbsync = ''
    ################################ SPOTIFY ##############################################
    # ACCOUNT INFORMATION
    IMAPAccount spotify-gmail
    # Address to connect to
    Host imap.gmail.com
    User zangao@spotify.com
    PassCmd "pass Email/spotify-mu-app.com"
    AuthMechs LOGIN
    SSLType IMAPS
    # SSLVersions SSLv3 # SSLv3 is deprecated, use default
    # ssl config for running mbsync as a launchd agent
    # CertificateFile /etc/ssl/certs/ca-certificates.crt
    # CertificateFile ~/.config/certificates/gmail.crt
    # CertificateFile /usr/local/etc/openssl@1.1/cert.pem

    # THEN WE SPECIFY THE LOCAL AND REMOTE STORAGE
    # - THE REMOTE STORAGE IS WHERE WE GET THE MAIL FROM (E.G., THE
    #   SPECIFICATION OF AN IMAP ACCOUNT)
    # - THE LOCAL STORAGE IS WHERE WE STORE THE EMAIL ON OUR COMPUTER

    # REMOTE STORAGE (USE THE IMAP ACCOUNT SPECIFIED ABOVE)
    IMAPStore spotify-remote
    Account spotify-gmail

    # LOCAL STORAGE (CREATE DIRECTORIES with mkdir -p ~/.mail)
    MaildirStore spotify-local
    Path ~/.mail/spotify/
    Inbox ~/.mail/spotify/INBOX

    # CONNECTIONS SPECIFY LINKS BETWEEN REMOTE AND LOCAL FOLDERS
    #
    # CONNECTIONS ARE SPECIFIED USING PATTERNS, WHICH MATCH REMOTE MAIl
    # FOLDERS. SOME COMMONLY USED PATTERS INCLUDE:
    #
    # 1 "*" TO MATCH EVERYTHING
    # 2 "!DIR" TO EXCLUDE "DIR"
    # 3 "DIR" TO MATCH DIR

    Channel spotify-inbox
    Far :spotify-remote:
    Near :spotify-local:
    Patterns "INBOX"
    Create Both
    Expunge Both
    SyncState *

    Channel spotify-sent
    # [Gmail]/Sent Mail doesn't seem to be the right remote folder
    Far :spotify-remote:"[Gmail]/Sent Mail"
    Near :spotify-local:"sent"
    Create Both
    Expunge Both
    SyncState *

    # Channel spotify-all
    # Far :spotify-remote:"[Gmail]/All Mail"
    # Near :spotify-local:"all"
    # Create Both
    # Expunge Both
    # SyncState *

    # GROUPS PUT TOGETHER CHANNELS, SO THAT WE CAN INVOKE
    # MBSYNC ON A GROUP TO SYNC ALL CHANNELS
    #
    # FOR INSTANCE: "mbsync gmail" GETS MAIL FROM
    # "spotify-inbox", "spotify-sent", and "gmail-trash"
    #
    Group spotifymail
    Channel spotify-inbox
    Channel spotify-sent
    # Channel spotify-all

    ################################ PERSONAL GMAIL ##############################################
    # ACCOUNT INFORMATION
    IMAPAccount gmail
    # Address to connect to
    Host imap.gmail.com
    User benny.ouattara@gmail.com
    PassCmd "pass Email/gmail-mu-app.com"
    AuthMechs LOGIN
    SSLType IMAPS
    # SSLVersions SSLv3 # deprecated, use default

    IMAPStore gmail-remote
    Account gmail

    MaildirStore gmail-local
    Path ~/.mail/gmail/
    Inbox ~/.mail/gmail/INBOX

    Channel gmail-inbox
    Far :gmail-remote:
    Near :gmail-local:
    Patterns "INBOX"
    Create Both
    Expunge Both
    SyncState *

    Channel gmail-sent
    Far :gmail-remote:"[Gmail]/Sent Mail"
    Near :gmail-local:"sent"
    Create Both
    Expunge Both
    SyncState *

    Group gmail
    Channel gmail-inbox
    Channel gmail-sent

    # ################################ PROTONMAIL ##############################################
    IMAPAccount protonmail
    Host 127.0.0.1
    Port 1143
    User benny.ouattara@protonmail.com
    # PassCmd "gpg --quiet --decrypt ~/.mbsync-pw-mailbox.gpg"
    PassCmd "pass Email/protonmail-mu-app.com"
    SSLType NONE
    #SSLVersions SSLv3
    #SSLVersions SSLv3
    # CertificateFile /etc/ssl/certs/ca-bundle.crt

    IMAPStore pm-remote
    Account protonmail

    MaildirStore pm-local
    Path ~/.mail/protonmail/
    Inbox ~/.mail/protonmail/INBOX

    Channel pm-inbox
    Far :pm-remote:
    Near :pm-local:
    Patterns "INBOX"
    Create Both
    Expunge Both
    SyncState *

    Channel pm-sent
    Far :pm-remote:"Sent"
    Near :pm-local:"sent"
    Create Both
    Expunge Both
    SyncState *

    Channel pm-trash
    Far :pm-remote:"Trash"
    Near :pm-local:"trash"
    Create Both
    Expunge Both
    SyncState *

    Channel pm-spam
    Far :pm-remote:"Spam"
    Near :pm-local:"spam"
    Create Both
    Expunge Both
    SyncState *

    Group protonmail
    Channel pm-inbox
    Channel pm-sent
    Channel pm-trash
    Channel pm-spam
  '';
}
