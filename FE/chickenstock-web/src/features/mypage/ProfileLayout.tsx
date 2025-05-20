import { useSimpleProfile } from "@/shared/model/queries";
import Profile from "@/features/dashboard/profile/ui/Profile";

const ProfileLayout = () => {
  const { data: simpleProfile } = useSimpleProfile();
  return <Profile simpleProfile={simpleProfile!} />;
};

export default ProfileLayout;
