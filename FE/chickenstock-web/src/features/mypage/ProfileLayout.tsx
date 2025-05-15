import { useEffect } from "react";
import { useSimpleProfile } from "@/shared/model/queries";
import Profile from "@/features/dashboard/profile/ui/Profile";

const ProfileLayout = () => {
  const { refetch: refetchSimpleProfile, data: simpleProfile } = useSimpleProfile();
  useEffect(() => {
    void refetchSimpleProfile();
  }, [refetchSimpleProfile]);
  return <Profile simpleProfile={simpleProfile!} />;
};

export default ProfileLayout;
