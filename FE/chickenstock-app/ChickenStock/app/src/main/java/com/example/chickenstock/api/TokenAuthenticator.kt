package com.example.chickenstock.api

import android.content.Context
import android.util.Log
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.viewmodel.AuthViewModel
import kotlinx.coroutines.runBlocking
import okhttp3.Authenticator
import okhttp3.Request
import okhttp3.Response
import okhttp3.Route
import java.util.concurrent.atomic.AtomicBoolean

/**
 * 토큰 인증 실패 시 재발급을 처리하는 Authenticator
 */
class TokenAuthenticator(
    private val context: Context
) : Authenticator {
    private val TAG = "TokenAuthenticator"
    
    // 토큰 재발급 중인지 확인하는 플래그
    companion object {
        private val isRefreshing = AtomicBoolean(false)
    }

    override fun authenticate(route: Route?, response: Response): Request? {
        // 자동 토큰 갱신 비활성화: 항상 null 반환
        return null
    }
} 