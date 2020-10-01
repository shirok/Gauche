/* 
 * 'system' certificate loader.  This is included from both tls.c and
 * tls-mbed.c. 
 */

#ifdef HAVE_WINCRYPT_H
static ScmObj system_cert_loader(ScmTLS *t,
                                 int (*mem_loader)(ScmTLS*, BYTE*, DWORD))
{
    const HCERTSTORE h = CertOpenStore(CERT_STORE_PROV_SYSTEM,
                                       X509_ASN_ENCODING,
                                       0,
                                       (CERT_STORE_SHARE_STORE_FLAG |
                                        CERT_STORE_SHARE_CONTEXT_FLAG |
                                        CERT_STORE_OPEN_EXISTING_FLAG |
                                        CERT_STORE_READONLY_FLAG |
                                        CERT_SYSTEM_STORE_LOCAL_MACHINE),
                                       TEXT("Root"));
    if (h == NULL) {
        Scm_Warn("Can't open certificate store");
        return SCM_FALSE;
    }

    if(!CertControlStore(h, 0, CERT_STORE_CTRL_AUTO_RESYNC, NULL)) {
        Scm_Warn("Can't resync certificate store");
        CertCloseStore(h, 0);
        return SCM_FALSE;
    }


    PCCERT_CONTEXT ctx = NULL;
    while(1) {
        ctx = CertEnumCertificatesInStore(h, ctx);

        if (ctx == NULL) { break; }

        int st = mem_loader(t, ctx->pbCertEncoded, ctx->cbCertEncoded);
        if(st != 0) {
            Scm_Warn("Certificate is not accepted: %d", st);
        }
    }

    CertCloseStore(h, 0);
    return SCM_TRUE;
}
#else
static ScmObj system_cert_loader(ScmTLS *t,
                                 int (*file_loader)(ScmTLS*, const char *))
{
    static const char *cacert_paths[] = {
        SYSTEM_CA_CERT_PATHS,
        NULL
    };
    static const char *cert_path = NULL;
    
    if (cert_path == NULL) {
        for (const char **p = cacert_paths; *p != NULL; p++) {
            int st = file_loader(t, *p);
            if (st == SSL_OK) {
                cert_path = *p;
                return SCM_TRUE;
            }
        }
    } else {
        int st = file_loader(t, cert_path);
        if (st == SSL_OK) return SCM_TRUE;
    }
    return SCM_FALSE;
}
#endif
