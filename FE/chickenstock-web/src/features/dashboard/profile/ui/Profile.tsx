import MyAsset from "@/features/dashboard/ui/MyAsset";
import UserInfo from "@/features/mypage/ui/UserInfo";
import { SimpleProfile } from "@/shared/store/types";

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
