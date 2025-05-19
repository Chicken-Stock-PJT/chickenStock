import MyAsset from "@/features/dashboard/ui/MyAsset";
import { SimpleProfile } from "@/features/mypage/model/types";
import UserInfo from "@/features/mypage/ui/UserInfo";

const Profile = ({ simpleProfile }: { simpleProfile: SimpleProfile }) => {
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
