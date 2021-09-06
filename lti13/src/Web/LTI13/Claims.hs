-- | Constant strings for various PURLs
module Web.LTI13.Claims where
import           Data.Text (Text)

claimMessageType :: Text
claimMessageType = "https://purl.imsglobal.org/spec/lti/claim/message_type"
claimVersion :: Text
claimVersion = "https://purl.imsglobal.org/spec/lti/claim/version"
claimDeploymentId :: Text
claimDeploymentId = "https://purl.imsglobal.org/spec/lti/claim/deployment_id"
claimTargetLinkUri :: Text
claimTargetLinkUri = "https://purl.imsglobal.org/spec/lti/claim/target_link_uri"
claimRoles :: Text
claimRoles = "https://purl.imsglobal.org/spec/lti/claim/roles"
claimContext :: Text
claimContext = "https://purl.imsglobal.org/spec/lti/claim/context"
claimLis :: Text
claimLis = "https://purl.imsglobal.org/spec/lti/claim/lis"
claimDlSettings :: Text
claimDlSettings = "https://purl.imsglobal.org/spec/lti-dl/claim/deep_linking_settings"
claimDlContentItems :: Text
claimDlContentItems = "https://purl.imsglobal.org/spec/lti-dl/claim/content_items"
claimDlData :: Text
claimDlData = "https://purl.imsglobal.org/spec/lti-dl/claim/data"
claimAgs :: Text
claimAgs = "https://purl.imsglobal.org/spec/lti-ags/claim/endpoint"

myLtiVersion :: Text
myLtiVersion = "1.3.0"

