# Reference tool setup

The IMS reference tool got wiped. This is a massive pain in the butt because it
validates nothing and has no examples so it will crash if handled with anything
but the delicate touch of a lesbian, and even that is often not enough based on
personal experience. So the wipe means I have to restore all the settings.

Going to document them here.

## Platform setup

In screenshot form:

![edit platform page screenshot. details are all below](./edit-platform.png)

### "Client"

This is the client ID (wat). It should be `abcde`.

### "Audience"

This is the issuer (bigger WAT). It should be `aaaaa`

### Private key

You have to put in a PEM private key to get it to not crash when you add a
resource link. A private key from `openssl keygen -out priv.key` is provided
here. Don't use this for anything.

```
-----BEGIN RSA PRIVATE KEY-----
MIIEowIBAAKCAQEAs0Q/px7z8y9RN3Vds4TA4LCS3uJl1TnFShuWSqGUZc1C8mzw
EfceUN1EY4Pv02+TwXCJeWRmnhKqb8Pf8K1Ln4BLp7oYEC33CteCOtFQmS3Wg2Fa
LitCRkYce+fiJ/O8CS0LjyhMc+4bfXE1jFm+hNNmFB2BWFHU+x28zNIOKyLAx2xu
yVDp2gYC5ig8xOy70yJoYiqpuh8v/FMzBMTeEz8z8wecolF2xiEh0gdpk5f8Gih1
NuKkM72vt1kheKbo850XsaFthbwKss/3DRDABEzfUqiQADJXv/BJ37+5++KHQ3Uz
2DpkRy4LnuJu4quj8qxklaOkDUIhYy9tGN3a9QIDAQABAoIBAAjl6VbtRWSbw6dC
Dx2bCEve19tyV2WnEXDlP6eCr7AB6UHJQH1Ty9BtuzhJUz0me6oNYPWCk0ljWcxk
Z5g1sdw/7QFtTkZ3UZzbJ56txxdjXBN3YJeMFCUS09UepG4hPgMilkFL2cejWP/3
/5Vw2vES4S08bbApl63DQxzTjFp9sNPgLLnXJWPzGY4/nrPOZSzhLb67iRkMAtwF
/jn3jaiNypDbjBH11KRWbkNdcwUpEku7NZw6IFI65rN+QWSCVTOOHSqkC2QKMGaD
Iq7RP9tag2z137PDN83P3UsXPxBwvODEm5wbP6e/9zvoqGwLEixfTkpf56GkBdKw
vPfB6AECgYEA4E8c+oMQEH0LyoCw/A5cPjYtST8MUth8sZGyB+x5/bDdBFx2VCi6
4aA1NOJfRdJDfSqCSB9+wJpkaEIdyfj3qVA8+zJaPF/IEIVSTu6vOC4t/szL2tN7
iIM2OdW5aWLnYQhzzsqY93yxBYTN64AOFeNYLA+0vfOCdWO9PasfegECgYEAzJgG
YFztkeYviW0qDigJhwj65WGAuTTO9voa5G2yE8PGR02vgh6fbYzuAMilWt+e/FC4
kHNoQL9rnvAAfFjopCR94WpUBFz+uVrvb8aPmAAdjnvYF1aCHe5zJhPl4TAYFaoW
ODf7ABxw/AngjCTg5vO/WndwChnGo622/jROGPUCgYEAhcip0AA7pJnwXmdDKBKo
kbJecEFaaE8WkBIFzRQN7nF+YQuEx81DKS8UjOY3TL2L4ytnaPzf7KApIsXMXppP
nHxu4qO1jizSdogN/2X8u9VPOZmMPIfGNjWMMhJ285Q5zHXuHHhZo8KLEm+TIdLO
k990x1LWyjRiqwFB7XYS4AECgYA+atr3JyKrckhqas17yCcv9UNmG5sD5eKGYsM1
kWnUsdOaCU9UKTeGWop53rzjvnlojM/h8fOKXVvd/2aGiTFgcmmdDa1YL27e6alG
tpM0UkSyJYJFjHTh4M9sfIGTc/LFGUDyIdIHtKWbnv/MgavDifWFCg86Zaa+ZM8B
Or/gyQKBgDOuiDCn2Uu/2RgR57hV5PgahxT3Itw7G7Ozjfgls/K0/LslQDMZa2AM
ghvn6gSzOQ9Nj/tOJSjqPS3mEBo04t2lq1NQo1Qg1RKfCEO/Ulw6RDEFuz6oo23L
U8FcgDhAKfwZAsOpbd96MAF+USH9KEU2+PvMKT62TEOnV/s0TMop
-----END RSA PRIVATE KEY-----
```

## Platform keys

Deployment ID: `deploydeploydeploy`

## Contexts/Courses

Create one of arbitrary label

## Resource links

**NOTE**: This might crash if your RSA privkey is not in PEM format (a format
used nowhere in the spec...)

Tool link URL: `http://localhost:3000/`

Login initiation URL: `http://localhost:3000/auth/page/lti13/initiate`


## Running a login

Go to the resource links page, and click **Select user for launch**. Then on a
user, click **Launch Resource Link (OIDC)** (the other button will not work; I
don't know what it actually is).

Expected stuff on this page is:

> iss: aaaaa
>
> login_hint: 321694 (random)
>
> target_link_uri: http://localhost:3000
>
> lti_message_hint: 32575 (random)
>
> lti_deployment_id: deploydeploydeploy
>
> client_id: abcde
