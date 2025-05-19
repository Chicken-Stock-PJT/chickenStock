import EditInfo from "./ui/EditInfo";
import EditPassword from "./ui/EditPassword";
import { useSimpleProfile } from "@/shared/model/queries";

const ProfileEdit = () => {
  const { data: profile } = useSimpleProfile();
  const showPasswordEdit = profile?.isOauth === "false";
  return (
    <div className="space-y-6 text-start">
      <EditInfo />
      {showPasswordEdit && <EditPassword />}
    </div>
  );
};

export default ProfileEdit;
