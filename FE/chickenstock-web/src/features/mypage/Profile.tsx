import { useEffect } from "react";
import MyAsset from "./ui/MyAsset";
import UserInfo from "./ui/UserInfo";
import { useSimpleProfile } from "@/shared/model/queries";
const Profile = () => {
  const { refetch: refetchSimpleProfile, data: simpleProfile } = useSimpleProfile();
  useEffect(() => {
    void refetchSimpleProfile();
  }, [refetchSimpleProfile]);
  return (
    <div className="space-y-6">
      {simpleProfile && (
        <>
          <UserInfo simpleProfile={simpleProfile} />
          <MyAsset
            totalAsset={Number(simpleProfile.totalAsset ?? 0)}
            memberMoney={Number(simpleProfile.memberMoney ?? 0)}
          />
        </>
      )}
    </div>
  );
};

export default Profile;
