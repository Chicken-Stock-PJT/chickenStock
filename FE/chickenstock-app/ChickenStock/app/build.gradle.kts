plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.android)
    alias(libs.plugins.kotlin.compose)
    id("com.google.devtools.ksp") version "2.1.0-1.0.29"
}

buildscript {
    dependencies {
        classpath("com.google.gms:google-services:4.4.1")
    }
}

android {
    namespace = "com.example.chickenstock"
    compileSdk = 35

    defaultConfig {
        applicationId = "com.example.chickenstock"
        minSdk = 26
        targetSdk = 35
        versionCode = 4
        versionName = "1.3"

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
        vectorDrawables {
            useSupportLibrary = true
        }
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
    kotlinOptions {
        jvmTarget = "17"
    }
    buildFeatures {
        compose = true
    }
    composeOptions {
        kotlinCompilerExtensionVersion = "1.5.1"
    }
    packaging {
        resources {
            excludes += "/META-INF/{AL2.0,LGPL2.1}"
        }
    }
    
    // Lint 오류 무시 설정 추가
    lint {
        abortOnError = false    // lint 오류가 발생해도 빌드가 중단되지 않도록 설정
        checkReleaseBuilds = false  // 릴리즈 빌드에서 lint 검사 비활성화
    }
}


dependencies {

    implementation(libs.androidx.core.ktx)
    implementation(libs.androidx.lifecycle.runtime.ktx)
    implementation(libs.androidx.activity.compose)
    implementation(platform(libs.androidx.compose.bom))
    implementation(libs.androidx.ui)
    implementation(libs.androidx.ui.graphics)
    implementation(libs.androidx.ui.tooling.preview)
    testImplementation(libs.junit)
    androidTestImplementation(libs.androidx.junit)
    androidTestImplementation(libs.androidx.espresso.core)
    androidTestImplementation(platform(libs.androidx.compose.bom))
    androidTestImplementation(libs.androidx.ui.test.junit4)
    debugImplementation(libs.androidx.ui.tooling)
    debugImplementation(libs.androidx.ui.test.manifest)
    implementation(libs.navigation.compose)
    implementation(libs.material3)
    implementation(libs.retrofit)
    implementation("com.squareup.retrofit2:converter-gson:2.9.0")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-android:1.7.3")
    implementation("androidx.compose.material:material-icons-extended")
    
    // Coil
    implementation("io.coil-kt:coil-compose:2.5.0")

    // MPAndroidChart
    implementation("com.github.PhilJay:MPAndroidChart:v3.1.0")

    // Retrofit
    implementation("com.squareup.retrofit2:retrofit:2.9.0")
    implementation("com.squareup.okhttp3:logging-interceptor:4.11.0")
    
    // Gson
    implementation("com.google.code.gson:gson:2.10.1")

    // 카카오 SDK
    implementation("com.kakao.sdk:v2-user:2.19.0")

    // 웹소켓 관련 의존성
    implementation("com.squareup.okhttp3:okhttp:4.12.0")

    // Accompanist Swipe Refresh
    implementation("com.google.accompanist:accompanist-swiperefresh:0.33.2-alpha")

    // Accompanist System UI Controller (상태바 색상 변경용)
    implementation("com.google.accompanist:accompanist-systemuicontroller:0.32.0")

    // Splashscreen
    implementation(libs.splashscreen)

    // Firebase Messaging
    implementation("com.google.firebase:firebase-messaging:23.4.1")
}

apply(plugin = "com.google.gms.google-services")