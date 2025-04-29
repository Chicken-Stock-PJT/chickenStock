import { useEffect } from "react";
import { useSearchParams } from "react-router-dom";
import axios from "axios";
import { useAuthStore } from "@/shared/store/auth";
import { getUserInfo } from "@/features/mypage/api";

const LoginRedirect = () => {
    const [searchParams] = useSearchParams();
    const code = searchParams.get('oneTimeCode');

    useEffect(() => {
        if (code) {
            const fetchData = async () => {
                try {
                    const response = await axios.post(`${import.meta.env.VITE_BASE_URL}/auth/exchange`, { oneTimeCode: code, platform: 'web' });
                    const accessToken = response.data.accessToken;
                    useAuthStore.getState().setAccessToken(accessToken);
                    useAuthStore.getState().getSimpleProfile();
                    window.location.href = '/';
                } catch (error) {
                    console.error(error);
                }
            };
            void fetchData();
        }
    }, [code]);  
    return (    
        <div>
            <h1>로그인 중...</h1>
        </div>
    )
}

export default LoginRedirect;