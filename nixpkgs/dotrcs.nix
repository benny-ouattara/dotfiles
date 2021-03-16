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
    SSLVersions SSLv3
    # ssl config for running mbsync as a launchd agent
    # CertificateFile /etc/ssl/certs/ca-certificates.crt
    CertificateFile ~/.config/certificates/gmail.crt

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
    Master :spotify-remote:
    Slave :spotify-local:
    Patterns "INBOX"
    Create Both
    Expunge Both
    SyncState *

    Channel spotify-sent
    # [Gmail]/Sent Mail doesn't seem to be the right remote folder
    Master :spotify-remote:"[Gmail]/Sent Mail"
    Slave :spotify-local:"sent"
    Create Both
    Expunge Both
    SyncState *

    # Channel spotify-all
    # Master :spotify-remote:"[Gmail]/All Mail"
    # Slave :spotify-local:"all"
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
    SSLVersions SSLv3

    IMAPStore gmail-remote
    Account gmail

    MaildirStore gmail-local
    Path ~/.mail/gmail/
    Inbox ~/.mail/gmail/INBOX

    Channel gmail-inbox
    Master :gmail-remote:
    Slave :gmail-local:
    Patterns "INBOX"
    Create Both
    Expunge Both
    SyncState *

    Channel gmail-sent
    Master :gmail-remote:"[Gmail]/Sent Mail"
    Slave :gmail-local:"sent"
    Create Both
    Expunge Both
    SyncState *

    Group gmail
    Channel gmail-inbox
    Channel gmail-sent

    ################################ PROTONMAIL ##############################################
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
    Master :pm-remote:
    Slave :pm-local:
    Patterns "INBOX"
    Create Both
    Expunge Both
    SyncState *

    Channel pm-sent
    Master :pm-remote:"Sent"
    Slave :pm-local:"sent"
    Create Both
    Expunge Both
    SyncState *

    Channel pm-trash
    Master :pm-remote:"Trash"
    Slave :pm-local:"trash"
    Create Both
    Expunge Both
    SyncState *

    Channel pm-spam
    Master :pm-remote:"Spam"
    Slave :pm-local:"spam"
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
