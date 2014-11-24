using System;

namespace PCA_Addin
{
    /// <summary>
    ///  Principal components analysis
    ///  Subroutine  builds  orthogonal  basis  where  first  axis  corresponds  to
    ///  direction with maximum variance, second axis maximizes variance in subspace
    ///  orthogonal to first axis and so on.
    ///  PCA does not use class labels.
    /// </summary>
    public partial class PCA_Method
    {
        /// <summary>
        /// Principal components analysis
        /// </summary>
        /// <param name="x">dataset, array[0..NPoints-1,0..NVars-1] matrix contains ONLY INDEPENDENT VARIABLES.</param>
        /// <param name="npoints">dataset size, NPoints>=0</param>
        /// <param name="nvars">number of independent variables, NVars>=1</param>
        /// <param name="info">return code: * -4, if SVD subroutine haven't converged
        ///                                 * -1, if wrong parameters has been passed (NPoints less 0, NVars less 1)
        ///                                 *  1, if task is solved
        /// </param>
        /// <param name="s2">array[0..NVars-1]. variance values corresponding to basis vectors.</param>
        /// <param name="v"> array[0..NVars-1,0..NVars-1] matrix, whose columns store basis vectors.</param>
        public void pcabuildbasis(double[,] x, int npoints, int nvars, out int info, out double[] s2, out double[,] v)
        {
            info = 0;
            s2 = new double[0];
            v = new double[0, 0];
            pca.pcabuildbasis(x, npoints, nvars, ref info, ref s2, ref v);
            cls_Results result = new cls_Results(info,s2,v);
            ThisAddIn.Go_QT.Lst_Results.Add(result);

            return;
        }
        /// <summary>
        ///  Principal components analysis
        /// </summary>
        public class pca
        {
            /*************************************************************************
            Subroutine  builds  orthogonal  basis  where  first  axis  corresponds  to
            direction with maximum variance, second axis maximizes variance in subspace
            orthogonal to first axis and so on.

            It should be noted that, unlike LDA, PCA does not use class labels.

            INPUT PARAMETERS:
                X           -   dataset, array[0..NPoints-1,0..NVars-1].
                                matrix contains ONLY INDEPENDENT VARIABLES.
                NPoints     -   dataset size, NPoints>=0
                NVars       -   number of independent variables, NVars>=1

            Output :
                Info        -   return code:
                                * -4, if SVD subroutine haven't converged
                                * -1, if wrong parameters has been passed (NPoints<0,
                                      NVars<1)
                                *  1, if task is solved
                S2          -   array[0..NVars-1]. variance values corresponding
                                to basis vectors.
                V           -   array[0..NVars-1,0..NVars-1]
                                matrix, whose columns store basis vectors.
            *************************************************************************/
            /// <summary>
            /// 
            /// </summary>
            /// <param name="x"></param>
            /// <param name="npoints"></param>
            /// <param name="nvars"></param>
            /// <param name="info"></param>
            /// <param name="s2"></param>
            /// <param name="v"></param>
            public static void pcabuildbasis(double[,] x, int npoints, int nvars, ref int info, ref double[] s2, ref double[,] v)
            {
                double[,] a = new double[0, 0];
                double[,] u = new double[0, 0];
                double[,] vt = new double[0, 0];
                double[] m = new double[0];
                double[] t = new double[0];
                int i = 0;
                int j = 0;
                double mean = 0;
                double variance = 0;
                double skewness = 0;
                double kurtosis = 0;
                int i_ = 0;

                info = 0;
                s2 = new double[0];
                v = new double[0, 0];

                //
                // Check input data
                //
                if (npoints < 0 | nvars < 1)
                {
                    info = -1;
                    return;
                }
                info = 1;

                //
                // Special case: NPoints=0
                //
                if (npoints == 0)
                {
                    s2 = new double[nvars - 1 + 1];
                    v = new double[nvars - 1 + 1, nvars - 1 + 1];
                    for (i = 0; i <= nvars - 1; i++)
                    {
                        s2[i] = 0;
                    }
                    for (i = 0; i <= nvars - 1; i++)
                    {
                        for (j = 0; j <= nvars - 1; j++)
                        {
                            if (i == j)
                            {
                                v[i, j] = 1;
                            }
                            else
                            {
                                v[i, j] = 0;
                            }
                        }
                    }
                    return;
                }

                //
                // Calculate means
                //
                m = new double[nvars - 1 + 1];
                t = new double[npoints - 1 + 1];

                for (j = 0; j <= nvars - 1; j++)
                {
                    for (i_ = 0; i_ <= npoints - 1; i_++)
                    {
                        try
                        {
                            t[i_] = x[i_, j];
                        }
                        catch { t[i_] = x[0, 0]; }    // ERROR-------------------------------------------------}
                    }
                    basestat.samplemoments(t, npoints, ref mean, ref variance, ref skewness, ref kurtosis);
                    m[j] = mean;
                }

                //
                // Center, apply SVD, prepare output
                //
                a = new double[Math.Max(npoints, nvars) - 1 + 1, nvars - 1 + 1];
                for (i = 0; i <= npoints - 1; i++)
                {
                    for (i_ = 0; i_ <= nvars - 1; i_++)
                    {
                        try
                        {
                            a[i, i_] = x[i, i_];
                        }
                        catch { a[i, i_] = x[0, 0]; }    //ERROR-------------------------------------------------}
                    }
                    for (i_ = 0; i_ <= nvars - 1; i_++)
                    {
                        a[i, i_] = a[i, i_] - m[i_];
                    }
                }

                for (i = npoints; i <= nvars - 1; i++)
                {
                    for (j = 0; j <= nvars - 1; j++)
                    {
                        a[i, j] = 0;
                    }
                }

                // Perform singular value decomposition
                if (!svd.rmatrixsvd(a, Math.Max(npoints, nvars), nvars, 0, 1, 2, ref s2, ref u, ref vt))
                {
                    info = -4;
                    return;
                }
                if (npoints != 1)
                {
                    for (i = 0; i <= nvars - 1; i++)
                    {
                        s2[i] = Math.Sqrt(s2[i]) / (npoints - 1);
                    }
                }
                v = new double[nvars - 1 + 1, nvars - 1 + 1];
                v = vt;
                
                //blas.copyandtranspose(vt, 0, nvars - 1, 0, nvars - 1, ref v, 0, nvars - 1, 0, nvars - 1);
               
            }
        }
        /// <summary>
        /// Calculation of the distribution moments: mean, variance, skewness, kurtosis
        /// </summary>
        public class basestat
        {
            /*************************************************************************
            Calculation of the distribution moments: mean, variance, skewness, kurtosis.

            INPUT PARAMETERS:
                X       -   sample
                N       -   N>=0, sample size:
                            * if given, only leading N elements of X are processed
                            * if not given, automatically determined from size of X
            
            OUTPUT PARAMETERS
                Mean    -   mean.
                Variance-   variance.
                Skewness-   skewness (if variance<>0; zero otherwise).
                Kurtosis-   kurtosis (if variance<>0; zero otherwise).


              -- ALGLIB --
                 Copyright 06.09.2006 by Bochkanov Sergey
            *************************************************************************/
            public static void samplemoments(double[] x,
                int n,
                ref double mean,
                ref double variance,
                ref double skewness,
                ref double kurtosis)
            {
                int i = 0;
                double v = 0;
                double v1 = 0;
                double v2 = 0;
                double stddev = 0;

                mean = 0;
                variance = 0;
                skewness = 0;
                kurtosis = 0;

                ap.assert(n >= 0, "SampleMoments: N<0");
                ap.assert(ap.len(x) >= n, "SampleMoments: Length(X)<N!");
                ap.assert(apserv.isfinitevector(x, n), "SampleMoments: X is not finite vector");
                //
                // Init, special case 'N=0'
                //
                mean = 0;
                variance = 0;
                skewness = 0;
                kurtosis = 0;
                stddev = 0;
                if (n <= 0)
                {
                    return;
                }

                //
                // Mean
                //
                for (i = 0; i <= n - 1; i++)
                {
                    mean = mean + x[i];
                }
                mean = mean / n;

                //
                // Variance (using corrected two-pass algorithm)
                //
                if (n != 1)
                {
                    v1 = 0;
                    for (i = 0; i <= n - 1; i++)
                    {
                        v1 = v1 + math.sqr(x[i] - mean);
                    }
                    v2 = 0;
                    for (i = 0; i <= n - 1; i++)
                    {
                        v2 = v2 + (x[i] - mean);
                    }
                    v2 = math.sqr(v2) / n;
                    variance = (v1 - v2) / (n - 1);
                    if ((double)(variance) < (double)(0))
                    {
                        variance = 0;
                    }
                    stddev = Math.Sqrt(variance);
                }

                //
                // Skewness and kurtosis
                //
                if ((double)(stddev) != (double)(0))
                {
                    for (i = 0; i <= n - 1; i++)
                    {
                        v = (x[i] - mean) / stddev;
                        v2 = math.sqr(v);
                        skewness = skewness + v2 * v;
                        kurtosis = kurtosis + math.sqr(v2);
                    }
                    skewness = skewness / n;
                    kurtosis = kurtosis / n - 3;
                }
            }


            /*************************************************************************
            ADev

            Input parameters:
                X   -   sample
                N   -   N>=0, sample size:
                        * if given, only leading N elements of X are processed
                        * if not given, automatically determined from size of X
            
            Output parameters:
                ADev-   ADev

            *************************************************************************/
            public static void sampleadev(double[] x,
                int n,
                ref double adev)
            {
                int i = 0;
                double mean = 0;

                adev = 0;

                ap.assert(n >= 0, "SampleADev: N<0");
                ap.assert(ap.len(x) >= n, "SampleADev: Length(X)<N!");
                ap.assert(apserv.isfinitevector(x, n), "SampleADev: X is not finite vector");

                //
                // Init, handle N=0
                //
                mean = 0;
                adev = 0;
                if (n <= 0)
                {
                    return;
                }

                //
                // Mean
                //
                for (i = 0; i <= n - 1; i++)
                {
                    mean = mean + x[i];
                }
                mean = mean / n;

                //
                // ADev
                //
                for (i = 0; i <= n - 1; i++)
                {
                    adev = adev + Math.Abs(x[i] - mean);
                }
                adev = adev / n;
            }


            /*************************************************************************
            Median calculation.

            Input parameters:
                X   -   sample (array indexes: [0..N-1])
                N   -   N>=0, sample size:
                        * if given, only leading N elements of X are processed
                        * if not given, automatically determined from size of X

            Output parameters:
                Median

            *************************************************************************/
            public static void samplemedian(double[] x,
                int n,
                ref double median)
            {
                int i = 0;
                int ir = 0;
                int j = 0;
                int l = 0;
                int midp = 0;
                int k = 0;
                double a = 0;
                double tval = 0;

                x = (double[])x.Clone();
                median = 0;

                ap.assert(n >= 0, "SampleMedian: N<0");
                ap.assert(ap.len(x) >= n, "SampleMedian: Length(X)<N!");
                ap.assert(apserv.isfinitevector(x, n), "SampleMedian: X is not finite vector");

                //
                // Some degenerate cases
                //
                median = 0;
                if (n <= 0)
                {
                    return;
                }
                if (n == 1)
                {
                    median = x[0];
                    return;
                }
                if (n == 2)
                {
                    median = 0.5 * (x[0] + x[1]);
                    return;
                }

                //
                // Common case, N>=3.
                // Choose X[(N-1)/2]
                //
                l = 0;
                ir = n - 1;
                k = (n - 1) / 2;
                while (true)
                {
                    if (ir <= l + 1)
                    {

                        //
                        // 1 or 2 elements in partition
                        //
                        if (ir == l + 1 & (double)(x[ir]) < (double)(x[l]))
                        {
                            tval = x[l];
                            x[l] = x[ir];
                            x[ir] = tval;
                        }
                        break;
                    }
                    else
                    {
                        midp = (l + ir) / 2;
                        tval = x[midp];
                        x[midp] = x[l + 1];
                        x[l + 1] = tval;
                        if ((double)(x[l]) > (double)(x[ir]))
                        {
                            tval = x[l];
                            x[l] = x[ir];
                            x[ir] = tval;
                        }
                        if ((double)(x[l + 1]) > (double)(x[ir]))
                        {
                            tval = x[l + 1];
                            x[l + 1] = x[ir];
                            x[ir] = tval;
                        }
                        if ((double)(x[l]) > (double)(x[l + 1]))
                        {
                            tval = x[l];
                            x[l] = x[l + 1];
                            x[l + 1] = tval;
                        }
                        i = l + 1;
                        j = ir;
                        a = x[l + 1];
                        while (true)
                        {
                            do
                            {
                                i = i + 1;
                            }
                            while ((double)(x[i]) < (double)(a));
                            do
                            {
                                j = j - 1;
                            }
                            while ((double)(x[j]) > (double)(a));
                            if (j < i)
                            {
                                break;
                            }
                            tval = x[i];
                            x[i] = x[j];
                            x[j] = tval;
                        }
                        x[l + 1] = x[j];
                        x[j] = a;
                        if (j >= k)
                        {
                            ir = j - 1;
                        }
                        if (j <= k)
                        {
                            l = i;
                        }
                    }
                }
                //
                // If N is odd, return result
                //
                if (n % 2 == 1)
                {
                    median = x[k];
                    return;
                }
                a = x[n - 1];
                for (i = k + 1; i <= n - 1; i++)
                {
                    if ((double)(x[i]) < (double)(a))
                    {
                        a = x[i];
                    }
                }
                median = 0.5 * (x[k] + a);
            }
            /*************************************************************************
            Percentile calculation.

            Input parameters:
                X   -   sample (array indexes: [0..N-1])
                N   -   N>=0, sample size:
                        * if given, only leading N elements of X are processed
                        * if not given, automatically determined from size of X
                P   -   percentile (0<=P<=1)

            Output parameters:
                V   -   percentile

              -- ALGLIB --
                 Copyright 01.03.2008 by Bochkanov Sergey
            *************************************************************************/
            public static void samplepercentile(double[] x,
                int n,
                double p,
                ref double v)
            {
                int i1 = 0;
                double t = 0;
                double[] rbuf = new double[0];

                x = (double[])x.Clone();
                v = 0;

                ap.assert(n >= 0, "SamplePercentile: N<0");
                ap.assert(ap.len(x) >= n, "SamplePercentile: Length(X)<N!");
                ap.assert(apserv.isfinitevector(x, n), "SamplePercentile: X is not finite vector");
                ap.assert(math.isfinite(p), "SamplePercentile: incorrect P!");
                ap.assert((double)(p) >= (double)(0) & (double)(p) <= (double)(1), "SamplePercentile: incorrect P!");
                tsort.tagsortfast(ref x, ref rbuf, n);
                if ((double)(p) == (double)(0))
                {
                    v = x[0];
                    return;
                }
                if ((double)(p) == (double)(1))
                {
                    v = x[n - 1];
                    return;
                }
                t = p * (n - 1);
                i1 = (int)Math.Floor(t);
                t = t - (int)Math.Floor(t);
                v = x[i1] * (1 - t) + x[i1 + 1] * t;
            }
            /*************************************************************************
            2-sample covariance

            Input parameters:
                X       -   sample 1 (array indexes: [0..N-1])
                Y       -   sample 2 (array indexes: [0..N-1])
                N       -   N>=0, sample size:
                            * if given, only N leading elements of X/Y are processed
                            * if not given, automatically determined from input sizes

            Result:
                covariance (zero for N=0 or N=1)

              -- ALGLIB --
                 Copyright 28.10.2010 by Bochkanov Sergey
            *************************************************************************/
            public static double cov2(double[] x,
                double[] y,
                int n)
            {
                double result = 0;
                int i = 0;
                double xmean = 0;
                double ymean = 0;
                double v = 0;
                double x0 = 0;
                double y0 = 0;
                double s = 0;
                bool samex = new bool();
                bool samey = new bool();

                ap.assert(n >= 0, "Cov2: N<0");
                ap.assert(ap.len(x) >= n, "Cov2: Length(X)<N!");
                ap.assert(ap.len(y) >= n, "Cov2: Length(Y)<N!");
                ap.assert(apserv.isfinitevector(x, n), "Cov2: X is not finite vector");
                ap.assert(apserv.isfinitevector(y, n), "Cov2: Y is not finite vector");

                //
                // Special case
                //
                if (n <= 1)
                {
                    result = 0;
                    return result;
                }

                //
                // Calculate mean.
                //
                //
                // Additonally we calculate SameX and SameY -
                // flag variables which are set to True when
                // all X[] (or Y[]) contain exactly same value.
                //
                // If at least one of them is True, we return zero
                // (othwerwise we risk to get nonzero covariation
                // because of roundoff).
                //
                xmean = 0;
                ymean = 0;
                samex = true;
                samey = true;
                x0 = x[0];
                y0 = y[0];
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    s = x[i];
                    samex = samex & (double)(s) == (double)(x0);
                    xmean = xmean + s * v;
                    s = y[i];
                    samey = samey & (double)(s) == (double)(y0);
                    ymean = ymean + s * v;
                }
                if (samex | samey)
                {
                    result = 0;
                    return result;
                }

                //
                // covariance
                //
                v = (double)1 / (double)(n - 1);
                result = 0;
                for (i = 0; i <= n - 1; i++)
                {
                    result = result + v * (x[i] - xmean) * (y[i] - ymean);
                }
                return result;
            }


            /*************************************************************************
            Pearson product-moment correlation coefficient

            Input parameters:
                X       -   sample 1 (array indexes: [0..N-1])
                Y       -   sample 2 (array indexes: [0..N-1])
                N       -   N>=0, sample size:
                            * if given, only N leading elements of X/Y are processed
                            * if not given, automatically determined from input sizes

            Result:
                Pearson product-moment correlation coefficient
                (zero for N=0 or N=1)

              -- ALGLIB --
                 Copyright 28.10.2010 by Bochkanov Sergey
            *************************************************************************/
            public static double pearsoncorr2(double[] x,
                double[] y,
                int n)
            {
                double result = 0;
                int i = 0;
                double xmean = 0;
                double ymean = 0;
                double v = 0;
                double x0 = 0;
                double y0 = 0;
                double s = 0;
                bool samex = new bool();
                bool samey = new bool();
                double xv = 0;
                double yv = 0;
                double t1 = 0;
                double t2 = 0;

                ap.assert(n >= 0, "PearsonCorr2: N<0");
                ap.assert(ap.len(x) >= n, "PearsonCorr2: Length(X)<N!");
                ap.assert(ap.len(y) >= n, "PearsonCorr2: Length(Y)<N!");
                ap.assert(apserv.isfinitevector(x, n), "PearsonCorr2: X is not finite vector");
                ap.assert(apserv.isfinitevector(y, n), "PearsonCorr2: Y is not finite vector");

                //
                // Special case
                //
                if (n <= 1)
                {
                    result = 0;
                    return result;
                }

                //
                // Calculate mean.
                //
                //
                // Additonally we calculate SameX and SameY -
                // flag variables which are set to True when
                // all X[] (or Y[]) contain exactly same value.
                //
                // If at least one of them is True, we return zero
                // (othwerwise we risk to get nonzero correlation
                // because of roundoff).
                //
                xmean = 0;
                ymean = 0;
                samex = true;
                samey = true;
                x0 = x[0];
                y0 = y[0];
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    s = x[i];
                    samex = samex & (double)(s) == (double)(x0);
                    xmean = xmean + s * v;
                    s = y[i];
                    samey = samey & (double)(s) == (double)(y0);
                    ymean = ymean + s * v;
                }
                if (samex | samey)
                {
                    result = 0;
                    return result;
                }

                //
                // numerator and denominator
                //
                s = 0;
                xv = 0;
                yv = 0;
                for (i = 0; i <= n - 1; i++)
                {
                    t1 = x[i] - xmean;
                    t2 = y[i] - ymean;
                    xv = xv + math.sqr(t1);
                    yv = yv + math.sqr(t2);
                    s = s + t1 * t2;
                }
                if ((double)(xv) == (double)(0) | (double)(yv) == (double)(0))
                {
                    result = 0;
                }
                else
                {
                    result = s / (Math.Sqrt(xv) * Math.Sqrt(yv));
                }
                return result;
            }


            /*************************************************************************
            Spearman's rank correlation coefficient

            Input parameters:
                X       -   sample 1 (array indexes: [0..N-1])
                Y       -   sample 2 (array indexes: [0..N-1])
                N       -   N>=0, sample size:
                            * if given, only N leading elements of X/Y are processed
                            * if not given, automatically determined from input sizes

            Result:
                Spearman's rank correlation coefficient
                (zero for N=0 or N=1)

            *************************************************************************/
            public static double spearmancorr2(double[] x,
                double[] y,
                int n)
            {
                double result = 0;
                apserv.apbuffers buf = new apserv.apbuffers();

                x = (double[])x.Clone();
                y = (double[])y.Clone();

                ap.assert(n >= 0, "SpearmanCorr2: N<0");
                ap.assert(ap.len(x) >= n, "SpearmanCorr2: Length(X)<N!");
                ap.assert(ap.len(y) >= n, "SpearmanCorr2: Length(Y)<N!");
                ap.assert(apserv.isfinitevector(x, n), "SpearmanCorr2: X is not finite vector");
                ap.assert(apserv.isfinitevector(y, n), "SpearmanCorr2: Y is not finite vector");

                //
                // Special case
                //
                if (n <= 1)
                {
                    result = 0;
                    return result;
                }
                basicstatops.rankx(ref x, n, buf);
                basicstatops.rankx(ref y, n, buf);
                result = pearsoncorr2(x, y, n);
                return result;
            }


            /*************************************************************************
            Covariance matrix

            INPUT PARAMETERS:
                X   -   array[N,M], sample matrix:
                        * J-th column corresponds to J-th variable
                        * I-th row corresponds to I-th observation
                N   -   N>=0, number of observations:
                        * if given, only leading N rows of X are used
                        * if not given, automatically determined from input size
                M   -   M>0, number of variables:
                        * if given, only leading M columns of X are used
                        * if not given, automatically determined from input size

            OUTPUT PARAMETERS:
                C   -   array[M,M], covariance matrix (zero if N=0 or N=1)

            *************************************************************************/
            public static void covm(double[,] x,
                int n,
                int m,
                ref double[,] c)
            {
                int i = 0;
                int j = 0;
                double v = 0;
                double[] t = new double[0];
                double[] x0 = new double[0];
                bool[] same = new bool[0];
                int i_ = 0;

                x = (double[,])x.Clone();
                c = new double[0, 0];

                ap.assert(n >= 0, "CovM: N<0");
                ap.assert(m >= 1, "CovM: M<1");
                ap.assert(ap.rows(x) >= n, "CovM: Rows(X)<N!");
                ap.assert(ap.cols(x) >= m | n == 0, "CovM: Cols(X)<M!");
                ap.assert(apserv.apservisfinitematrix(x, n, m), "CovM: X contains infinite/NAN elements");

                //
                // N<=1, return zero
                //
                if (n <= 1)
                {
                    c = new double[m, m];
                    for (i = 0; i <= m - 1; i++)
                    {
                        for (j = 0; j <= m - 1; j++)
                        {
                            c[i, j] = 0;
                        }
                    }
                    return;
                }

                //
                // Calculate means,
                // check for constant columns
                //
                t = new double[m];
                x0 = new double[m];
                same = new bool[m];
                c = new double[m, m];
                for (i = 0; i <= m - 1; i++)
                {
                    t[i] = 0;
                    same[i] = true;
                }
                for (i_ = 0; i_ <= m - 1; i_++)
                {
                    x0[i_] = x[0, i_];
                }
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m - 1; i_++)
                    {
                        t[i_] = t[i_] + v * x[i, i_];
                    }
                    for (j = 0; j <= m - 1; j++)
                    {
                        same[j] = same[j] & (double)(x[i, j]) == (double)(x0[j]);
                    }
                }

                //
                // * center variables;
                // * if we have constant columns, these columns are
                //   artificially zeroed (they must be zero in exact arithmetics,
                //   but unfortunately floating point ops are not exact).
                // * calculate upper half of symmetric covariance matrix
                //
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m - 1; i_++)
                    {
                        x[i, i_] = x[i, i_] - t[i_];
                    }
                    for (j = 0; j <= m - 1; j++)
                    {
                        if (same[j])
                        {
                            x[i, j] = 0;
                        }
                    }
                }
                ablas.rmatrixsyrk(m, n, (double)1 / (double)(n - 1), x, 0, 0, 1, 0.0, ref c, 0, 0, true);

                //
                // force symmetricity
                //
                for (i = 0; i <= m - 2; i++)
                {
                    for (i_ = i + 1; i_ <= m - 1; i_++)
                    {
                        c[i_, i] = c[i, i_];
                    }
                }
            }


            /*************************************************************************
            Pearson product-moment correlation matrix

            INPUT PARAMETERS:
                X   -   array[N,M], sample matrix:
                        * J-th column corresponds to J-th variable
                        * I-th row corresponds to I-th observation
                N   -   N>=0, number of observations:
                        * if given, only leading N rows of X are used
                        * if not given, automatically determined from input size
                M   -   M>0, number of variables:
                        * if given, only leading M columns of X are used
                        * if not given, automatically determined from input size

            OUTPUT PARAMETERS:
                C   -   array[M,M], correlation matrix (zero if N=0 or N=1)

              -- ALGLIB --
                 Copyright 28.10.2010 by Bochkanov Sergey
            *************************************************************************/
            public static void pearsoncorrm(double[,] x,
                int n,
                int m,
                ref double[,] c)
            {
                double[] t = new double[0];
                int i = 0;
                int j = 0;

                c = new double[0, 0];

                ap.assert(n >= 0, "PearsonCorrM: N<0");
                ap.assert(m >= 1, "PearsonCorrM: M<1");
                ap.assert(ap.rows(x) >= n, "PearsonCorrM: Rows(X)<N!");
                ap.assert(ap.cols(x) >= m | n == 0, "PearsonCorrM: Cols(X)<M!");
                ap.assert(apserv.apservisfinitematrix(x, n, m), "PearsonCorrM: X contains infinite/NAN elements");
                t = new double[m];
                covm(x, n, m, ref c);
                for (i = 0; i <= m - 1; i++)
                {
                    t[i] = Math.Sqrt(c[i, i]);
                }
                for (i = 0; i <= m - 1; i++)
                {
                    for (j = 0; j <= m - 1; j++)
                    {
                        if ((double)(t[i]) != (double)(0) & (double)(t[j]) != (double)(0))
                        {
                            c[i, j] = c[i, j] / (t[i] * t[j]);
                        }
                        else
                        {
                            c[i, j] = 0.0;
                        }
                    }
                }
            }


            /*************************************************************************
            Spearman's rank correlation matrix

            INPUT PARAMETERS:
                X   -   array[N,M], sample matrix:
                        * J-th column corresponds to J-th variable
                        * I-th row corresponds to I-th observation
                N   -   N>=0, number of observations:
                        * if given, only leading N rows of X are used
                        * if not given, automatically determined from input size
                M   -   M>0, number of variables:
                        * if given, only leading M columns of X are used
                        * if not given, automatically determined from input size

            OUTPUT PARAMETERS:
                C   -   array[M,M], correlation matrix (zero if N=0 or N=1)

              -- ALGLIB --
                 Copyright 28.10.2010 by Bochkanov Sergey
            *************************************************************************/
            public static void spearmancorrm(double[,] x,
                int n,
                int m,
                ref double[,] c)
            {
                int i = 0;
                int j = 0;
                apserv.apbuffers buf = new apserv.apbuffers();
                double[] t = new double[0];
                double v = 0;
                double[] x0 = new double[0];
                bool[] same = new bool[0];
                int i_ = 0;

                x = (double[,])x.Clone();
                c = new double[0, 0];

                ap.assert(n >= 0, "SpearmanCorrM: N<0");
                ap.assert(m >= 1, "SpearmanCorrM: M<1");
                ap.assert(ap.rows(x) >= n, "SpearmanCorrM: Rows(X)<N!");
                ap.assert(ap.cols(x) >= m | n == 0, "SpearmanCorrM: Cols(X)<M!");
                ap.assert(apserv.apservisfinitematrix(x, n, m), "SpearmanCorrM: X contains infinite/NAN elements");

                //
                // N<=1, return zero
                //
                if (n <= 1)
                {
                    c = new double[m, m];
                    for (i = 0; i <= m - 1; i++)
                    {
                        for (j = 0; j <= m - 1; j++)
                        {
                            c[i, j] = 0;
                        }
                    }
                    return;
                }

                //
                // Allocate
                //
                t = new double[Math.Max(n, m)];
                x0 = new double[m];
                same = new bool[m];
                c = new double[m, m];

                //
                // Replace data with ranks
                //
                for (j = 0; j <= m - 1; j++)
                {
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        t[i_] = x[i_, j];
                    }
                    basicstatops.rankx(ref t, n, buf);
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        x[i_, j] = t[i_];
                    }
                }

                //
                // Calculate means,
                // check for constant columns
                //
                for (i = 0; i <= m - 1; i++)
                {
                    t[i] = 0;
                    same[i] = true;
                }
                for (i_ = 0; i_ <= m - 1; i_++)
                {
                    x0[i_] = x[0, i_];
                }
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m - 1; i_++)
                    {
                        t[i_] = t[i_] + v * x[i, i_];
                    }
                    for (j = 0; j <= m - 1; j++)
                    {
                        same[j] = same[j] & (double)(x[i, j]) == (double)(x0[j]);
                    }
                }

                //
                // * center variables;
                // * if we have constant columns, these columns are
                //   artificialy zeroed (they must be zero in exact arithmetics,
                //   but unfortunately floating point is not exact).
                // * calculate upper half of symmetric covariance matrix
                //
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m - 1; i_++)
                    {
                        x[i, i_] = x[i, i_] - t[i_];
                    }
                    for (j = 0; j <= m - 1; j++)
                    {
                        if (same[j])
                        {
                            x[i, j] = 0;
                        }
                    }
                }
                ablas.rmatrixsyrk(m, n, (double)1 / (double)(n - 1), x, 0, 0, 1, 0.0, ref c, 0, 0, true);

                //
                // force symmetricity
                //
                for (i = 0; i <= m - 2; i++)
                {
                    for (i_ = i + 1; i_ <= m - 1; i_++)
                    {
                        c[i_, i] = c[i, i_];
                    }
                }

                //
                // Calculate Pearson coefficients
                //
                for (i = 0; i <= m - 1; i++)
                {
                    t[i] = Math.Sqrt(c[i, i]);
                }
                for (i = 0; i <= m - 1; i++)
                {
                    for (j = 0; j <= m - 1; j++)
                    {
                        if ((double)(t[i]) != (double)(0) & (double)(t[j]) != (double)(0))
                        {
                            c[i, j] = c[i, j] / (t[i] * t[j]);
                        }
                        else
                        {
                            c[i, j] = 0.0;
                        }
                    }
                }
            }


            /*************************************************************************
            Cross-covariance matrix

            INPUT PARAMETERS:
                X   -   array[N,M1], sample matrix:
                        * J-th column corresponds to J-th variable
                        * I-th row corresponds to I-th observation
                Y   -   array[N,M2], sample matrix:
                        * J-th column corresponds to J-th variable
                        * I-th row corresponds to I-th observation
                N   -   N>=0, number of observations:
                        * if given, only leading N rows of X/Y are used
                        * if not given, automatically determined from input sizes
                M1  -   M1>0, number of variables in X:
                        * if given, only leading M1 columns of X are used
                        * if not given, automatically determined from input size
                M2  -   M2>0, number of variables in Y:
                        * if given, only leading M1 columns of X are used
                        * if not given, automatically determined from input size

            OUTPUT PARAMETERS:
                C   -   array[M1,M2], cross-covariance matrix (zero if N=0 or N=1)

              -- ALGLIB --
                 Copyright 28.10.2010 by Bochkanov Sergey
            *************************************************************************/
            public static void covm2(double[,] x,
                double[,] y,
                int n,
                int m1,
                int m2,
                ref double[,] c)
            {
                int i = 0;
                int j = 0;
                double v = 0;
                double[] t = new double[0];
                double[] x0 = new double[0];
                double[] y0 = new double[0];
                bool[] samex = new bool[0];
                bool[] samey = new bool[0];
                int i_ = 0;

                x = (double[,])x.Clone();
                y = (double[,])y.Clone();
                c = new double[0, 0];

                ap.assert(n >= 0, "CovM2: N<0");
                ap.assert(m1 >= 1, "CovM2: M1<1");
                ap.assert(m2 >= 1, "CovM2: M2<1");
                ap.assert(ap.rows(x) >= n, "CovM2: Rows(X)<N!");
                ap.assert(ap.cols(x) >= m1 | n == 0, "CovM2: Cols(X)<M1!");
                ap.assert(apserv.apservisfinitematrix(x, n, m1), "CovM2: X contains infinite/NAN elements");
                ap.assert(ap.rows(y) >= n, "CovM2: Rows(Y)<N!");
                ap.assert(ap.cols(y) >= m2 | n == 0, "CovM2: Cols(Y)<M2!");
                ap.assert(apserv.apservisfinitematrix(y, n, m2), "CovM2: X contains infinite/NAN elements");

                //
                // N<=1, return zero
                //
                if (n <= 1)
                {
                    c = new double[m1, m2];
                    for (i = 0; i <= m1 - 1; i++)
                    {
                        for (j = 0; j <= m2 - 1; j++)
                        {
                            c[i, j] = 0;
                        }
                    }
                    return;
                }

                //
                // Allocate
                //
                t = new double[Math.Max(m1, m2)];
                x0 = new double[m1];
                y0 = new double[m2];
                samex = new bool[m1];
                samey = new bool[m2];
                c = new double[m1, m2];

                //
                // * calculate means of X
                // * center X
                // * if we have constant columns, these columns are
                //   artificially zeroed (they must be zero in exact arithmetics,
                //   but unfortunately floating point ops are not exact).
                //
                for (i = 0; i <= m1 - 1; i++)
                {
                    t[i] = 0;
                    samex[i] = true;
                }
                for (i_ = 0; i_ <= m1 - 1; i_++)
                {
                    x0[i_] = x[0, i_];
                }
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m1 - 1; i_++)
                    {
                        t[i_] = t[i_] + v * x[i, i_];
                    }
                    for (j = 0; j <= m1 - 1; j++)
                    {
                        samex[j] = samex[j] & (double)(x[i, j]) == (double)(x0[j]);
                    }
                }
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m1 - 1; i_++)
                    {
                        x[i, i_] = x[i, i_] - t[i_];
                    }
                    for (j = 0; j <= m1 - 1; j++)
                    {
                        if (samex[j])
                        {
                            x[i, j] = 0;
                        }
                    }
                }

                //
                // Repeat same steps for Y
                //
                for (i = 0; i <= m2 - 1; i++)
                {
                    t[i] = 0;
                    samey[i] = true;
                }
                for (i_ = 0; i_ <= m2 - 1; i_++)
                {
                    y0[i_] = y[0, i_];
                }
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m2 - 1; i_++)
                    {
                        t[i_] = t[i_] + v * y[i, i_];
                    }
                    for (j = 0; j <= m2 - 1; j++)
                    {
                        samey[j] = samey[j] & (double)(y[i, j]) == (double)(y0[j]);
                    }
                }
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m2 - 1; i_++)
                    {
                        y[i, i_] = y[i, i_] - t[i_];
                    }
                    for (j = 0; j <= m2 - 1; j++)
                    {
                        if (samey[j])
                        {
                            y[i, j] = 0;
                        }
                    }
                }

                //
                // calculate cross-covariance matrix
                //
                ablas.rmatrixgemm(m1, m2, n, (double)1 / (double)(n - 1), x, 0, 0, 1, y, 0, 0, 0, 0.0, ref c, 0, 0);
            }


            /*************************************************************************
            Pearson product-moment cross-correlation matrix

            INPUT PARAMETERS:
                X   -   array[N,M1], sample matrix:
                        * J-th column corresponds to J-th variable
                        * I-th row corresponds to I-th observation
                Y   -   array[N,M2], sample matrix:
                        * J-th column corresponds to J-th variable
                        * I-th row corresponds to I-th observation
                N   -   N>=0, number of observations:
                        * if given, only leading N rows of X/Y are used
                        * if not given, automatically determined from input sizes
                M1  -   M1>0, number of variables in X:
                        * if given, only leading M1 columns of X are used
                        * if not given, automatically determined from input size
                M2  -   M2>0, number of variables in Y:
                        * if given, only leading M1 columns of X are used
                        * if not given, automatically determined from input size

            OUTPUT PARAMETERS:
                C   -   array[M1,M2], cross-correlation matrix (zero if N=0 or N=1)

              -- ALGLIB --
                 Copyright 28.10.2010 by Bochkanov Sergey
            *************************************************************************/
            public static void pearsoncorrm2(double[,] x,
                double[,] y,
                int n,
                int m1,
                int m2,
                ref double[,] c)
            {
                int i = 0;
                int j = 0;
                double v = 0;
                double[] t = new double[0];
                double[] x0 = new double[0];
                double[] y0 = new double[0];
                double[] sx = new double[0];
                double[] sy = new double[0];
                bool[] samex = new bool[0];
                bool[] samey = new bool[0];
                int i_ = 0;

                x = (double[,])x.Clone();
                y = (double[,])y.Clone();
                c = new double[0, 0];

                ap.assert(n >= 0, "PearsonCorrM2: N<0");
                ap.assert(m1 >= 1, "PearsonCorrM2: M1<1");
                ap.assert(m2 >= 1, "PearsonCorrM2: M2<1");
                ap.assert(ap.rows(x) >= n, "PearsonCorrM2: Rows(X)<N!");
                ap.assert(ap.cols(x) >= m1 | n == 0, "PearsonCorrM2: Cols(X)<M1!");
                ap.assert(apserv.apservisfinitematrix(x, n, m1), "PearsonCorrM2: X contains infinite/NAN elements");
                ap.assert(ap.rows(y) >= n, "PearsonCorrM2: Rows(Y)<N!");
                ap.assert(ap.cols(y) >= m2 | n == 0, "PearsonCorrM2: Cols(Y)<M2!");
                ap.assert(apserv.apservisfinitematrix(y, n, m2), "PearsonCorrM2: X contains infinite/NAN elements");

                //
                // N<=1, return zero
                //
                if (n <= 1)
                {
                    c = new double[m1, m2];
                    for (i = 0; i <= m1 - 1; i++)
                    {
                        for (j = 0; j <= m2 - 1; j++)
                        {
                            c[i, j] = 0;
                        }
                    }
                    return;
                }

                //
                // Allocate
                //
                t = new double[Math.Max(m1, m2)];
                x0 = new double[m1];
                y0 = new double[m2];
                sx = new double[m1];
                sy = new double[m2];
                samex = new bool[m1];
                samey = new bool[m2];
                c = new double[m1, m2];

                //
                // * calculate means of X
                // * center X
                // * if we have constant columns, these columns are
                //   artificially zeroed (they must be zero in exact arithmetics,
                //   but unfortunately floating point ops are not exact).
                // * calculate column variances
                //
                for (i = 0; i <= m1 - 1; i++)
                {
                    t[i] = 0;
                    samex[i] = true;
                    sx[i] = 0;
                }
                for (i_ = 0; i_ <= m1 - 1; i_++)
                {
                    x0[i_] = x[0, i_];
                }
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m1 - 1; i_++)
                    {
                        t[i_] = t[i_] + v * x[i, i_];
                    }
                    for (j = 0; j <= m1 - 1; j++)
                    {
                        samex[j] = samex[j] & (double)(x[i, j]) == (double)(x0[j]);
                    }
                }
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m1 - 1; i_++)
                    {
                        x[i, i_] = x[i, i_] - t[i_];
                    }
                    for (j = 0; j <= m1 - 1; j++)
                    {
                        if (samex[j])
                        {
                            x[i, j] = 0;
                        }
                        sx[j] = sx[j] + x[i, j] * x[i, j];
                    }
                }
                for (j = 0; j <= m1 - 1; j++)
                {
                    sx[j] = Math.Sqrt(sx[j] / (n - 1));
                }

                //
                // Repeat same steps for Y
                //
                for (i = 0; i <= m2 - 1; i++)
                {
                    t[i] = 0;
                    samey[i] = true;
                    sy[i] = 0;
                }
                for (i_ = 0; i_ <= m2 - 1; i_++)
                {
                    y0[i_] = y[0, i_];
                }
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m2 - 1; i_++)
                    {
                        t[i_] = t[i_] + v * y[i, i_];
                    }
                    for (j = 0; j <= m2 - 1; j++)
                    {
                        samey[j] = samey[j] & (double)(y[i, j]) == (double)(y0[j]);
                    }
                }
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m2 - 1; i_++)
                    {
                        y[i, i_] = y[i, i_] - t[i_];
                    }
                    for (j = 0; j <= m2 - 1; j++)
                    {
                        if (samey[j])
                        {
                            y[i, j] = 0;
                        }
                        sy[j] = sy[j] + y[i, j] * y[i, j];
                    }
                }
                for (j = 0; j <= m2 - 1; j++)
                {
                    sy[j] = Math.Sqrt(sy[j] / (n - 1));
                }

                //
                // calculate cross-covariance matrix
                //
                ablas.rmatrixgemm(m1, m2, n, (double)1 / (double)(n - 1), x, 0, 0, 1, y, 0, 0, 0, 0.0, ref c, 0, 0);

                //
                // Divide by standard deviations
                //
                for (i = 0; i <= m1 - 1; i++)
                {
                    for (j = 0; j <= m2 - 1; j++)
                    {
                        if ((double)(sx[i]) != (double)(0) & (double)(sy[j]) != (double)(0))
                        {
                            c[i, j] = c[i, j] / (sx[i] * sy[j]);
                        }
                        else
                        {
                            c[i, j] = 0;
                        }
                    }
                }
            }


            /*************************************************************************
            Spearman's rank cross-correlation matrix

            INPUT PARAMETERS:
                X   -   array[N,M1], sample matrix:
                        * J-th column corresponds to J-th variable
                        * I-th row corresponds to I-th observation
                Y   -   array[N,M2], sample matrix:
                        * J-th column corresponds to J-th variable
                        * I-th row corresponds to I-th observation
                N   -   N>=0, number of observations:
                        * if given, only leading N rows of X/Y are used
                        * if not given, automatically determined from input sizes
                M1  -   M1>0, number of variables in X:
                        * if given, only leading M1 columns of X are used
                        * if not given, automatically determined from input size
                M2  -   M2>0, number of variables in Y:
                        * if given, only leading M1 columns of X are used
                        * if not given, automatically determined from input size

            OUTPUT PARAMETERS:
                C   -   array[M1,M2], cross-correlation matrix (zero if N=0 or N=1)

              -- ALGLIB --
                 Copyright 28.10.2010 by Bochkanov Sergey
            *************************************************************************/
            public static void spearmancorrm2(double[,] x,
                double[,] y,
                int n,
                int m1,
                int m2,
                ref double[,] c)
            {
                int i = 0;
                int j = 0;
                double v = 0;
                double[] t = new double[0];
                double[] x0 = new double[0];
                double[] y0 = new double[0];
                double[] sx = new double[0];
                double[] sy = new double[0];
                bool[] samex = new bool[0];
                bool[] samey = new bool[0];
                apserv.apbuffers buf = new apserv.apbuffers();
                int i_ = 0;

                x = (double[,])x.Clone();
                y = (double[,])y.Clone();
                c = new double[0, 0];

                ap.assert(n >= 0, "SpearmanCorrM2: N<0");
                ap.assert(m1 >= 1, "SpearmanCorrM2: M1<1");
                ap.assert(m2 >= 1, "SpearmanCorrM2: M2<1");
                ap.assert(ap.rows(x) >= n, "SpearmanCorrM2: Rows(X)<N!");
                ap.assert(ap.cols(x) >= m1 | n == 0, "SpearmanCorrM2: Cols(X)<M1!");
                ap.assert(apserv.apservisfinitematrix(x, n, m1), "SpearmanCorrM2: X contains infinite/NAN elements");
                ap.assert(ap.rows(y) >= n, "SpearmanCorrM2: Rows(Y)<N!");
                ap.assert(ap.cols(y) >= m2 | n == 0, "SpearmanCorrM2: Cols(Y)<M2!");
                ap.assert(apserv.apservisfinitematrix(y, n, m2), "SpearmanCorrM2: X contains infinite/NAN elements");

                //
                // N<=1, return zero
                //
                if (n <= 1)
                {
                    c = new double[m1, m2];
                    for (i = 0; i <= m1 - 1; i++)
                    {
                        for (j = 0; j <= m2 - 1; j++)
                        {
                            c[i, j] = 0;
                        }
                    }
                    return;
                }

                //
                // Allocate
                //
                t = new double[Math.Max(Math.Max(m1, m2), n)];
                x0 = new double[m1];
                y0 = new double[m2];
                sx = new double[m1];
                sy = new double[m2];
                samex = new bool[m1];
                samey = new bool[m2];
                c = new double[m1, m2];

                //
                // Replace data with ranks
                //
                for (j = 0; j <= m1 - 1; j++)
                {
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        t[i_] = x[i_, j];
                    }
                    basicstatops.rankx(ref t, n, buf);
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        x[i_, j] = t[i_];
                    }
                }
                for (j = 0; j <= m2 - 1; j++)
                {
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        t[i_] = y[i_, j];
                    }
                    basicstatops.rankx(ref t, n, buf);
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        y[i_, j] = t[i_];
                    }
                }

                //
                // * calculate means of X
                // * center X
                // * if we have constant columns, these columns are
                //   artificially zeroed (they must be zero in exact arithmetics,
                //   but unfortunately floating point ops are not exact).
                // * calculate column variances
                //
                for (i = 0; i <= m1 - 1; i++)
                {
                    t[i] = 0;
                    samex[i] = true;
                    sx[i] = 0;
                }
                for (i_ = 0; i_ <= m1 - 1; i_++)
                {
                    x0[i_] = x[0, i_];
                }
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m1 - 1; i_++)
                    {
                        t[i_] = t[i_] + v * x[i, i_];
                    }
                    for (j = 0; j <= m1 - 1; j++)
                    {
                        samex[j] = samex[j] & (double)(x[i, j]) == (double)(x0[j]);
                    }
                }
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m1 - 1; i_++)
                    {
                        x[i, i_] = x[i, i_] - t[i_];
                    }
                    for (j = 0; j <= m1 - 1; j++)
                    {
                        if (samex[j])
                        {
                            x[i, j] = 0;
                        }
                        sx[j] = sx[j] + x[i, j] * x[i, j];
                    }
                }
                for (j = 0; j <= m1 - 1; j++)
                {
                    sx[j] = Math.Sqrt(sx[j] / (n - 1));
                }

                //
                // Repeat same steps for Y
                //
                for (i = 0; i <= m2 - 1; i++)
                {
                    t[i] = 0;
                    samey[i] = true;
                    sy[i] = 0;
                }
                for (i_ = 0; i_ <= m2 - 1; i_++)
                {
                    y0[i_] = y[0, i_];
                }
                v = (double)1 / (double)n;
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m2 - 1; i_++)
                    {
                        t[i_] = t[i_] + v * y[i, i_];
                    }
                    for (j = 0; j <= m2 - 1; j++)
                    {
                        samey[j] = samey[j] & (double)(y[i, j]) == (double)(y0[j]);
                    }
                }
                for (i = 0; i <= n - 1; i++)
                {
                    for (i_ = 0; i_ <= m2 - 1; i_++)
                    {
                        y[i, i_] = y[i, i_] - t[i_];
                    }
                    for (j = 0; j <= m2 - 1; j++)
                    {
                        if (samey[j])
                        {
                            y[i, j] = 0;
                        }
                        sy[j] = sy[j] + y[i, j] * y[i, j];
                    }
                }
                for (j = 0; j <= m2 - 1; j++)
                {
                    sy[j] = Math.Sqrt(sy[j] / (n - 1));
                }

                //
                // calculate cross-covariance matrix
                //
                ablas.rmatrixgemm(m1, m2, n, (double)1 / (double)(n - 1), x, 0, 0, 1, y, 0, 0, 0, 0.0, ref c, 0, 0);

                //
                // Divide by standard deviations
                //
                for (i = 0; i <= m1 - 1; i++)
                {
                    for (j = 0; j <= m2 - 1; j++)
                    {
                        if ((double)(sx[i]) != (double)(0) & (double)(sy[j]) != (double)(0))
                        {
                            c[i, j] = c[i, j] / (sx[i] * sy[j]);
                        }
                        else
                        {
                            c[i, j] = 0;
                        }
                    }
                }
            }


            /*************************************************************************
            Obsolete function, we recommend to use PearsonCorr2().

              -- ALGLIB --
                 Copyright 09.04.2007 by Bochkanov Sergey
            *************************************************************************/
            public static double pearsoncorrelation(double[] x,
                double[] y,
                int n)
            {
                double result = 0;

                result = pearsoncorr2(x, y, n);
                return result;
            }


            /*************************************************************************
            Obsolete function, we recommend to use SpearmanCorr2().

                -- ALGLIB --
                Copyright 09.04.2007 by Bochkanov Sergey
            *************************************************************************/
            public static double spearmanrankcorrelation(double[] x,
                double[] y,
                int n)
            {
                double result = 0;

                result = spearmancorr2(x, y, n);
                return result;
            }


        }
        /********************************************************************
     internal functions
     ********************************************************************/
        public class ap
        {
            public static int len<T>(T[] a)
            { return a.Length; }
            public static int rows<T>(T[,] a)
            { return a.GetLength(0); }
            public static int cols<T>(T[,] a)
            { return a.GetLength(1); }
            public static void swap<T>(ref T a, ref T b)
            {
                T t = a;
                a = b;
                b = t;
            }

            public static void assert(bool cond, string s)
            {
                if (!cond)
                {
                    //   throw new alglibexception(s);
                }
            }

            public static void assert(bool cond)
            {
                assert(cond, "ALGLIB: assertion failed");
            }

            /****************************************************************
            returns dps (digits-of-precision) value corresponding to threshold.
            dps(0.9)  = dps(0.5)  = dps(0.1) = 0
            dps(0.09) = dps(0.05) = dps(0.01) = 1
            and so on
            ****************************************************************/
            public static int threshold2dps(double threshold)
            {
                int result = 0;
                double t;
                for (result = 0, t = 1; t / 10 > threshold * (1 + 1E-10); result++, t /= 10) ;
                return result;
            }

            /****************************************************************
            prints formatted complex
            ****************************************************************/
            public static string format(complex a, int _dps)
            {
                int dps = Math.Abs(_dps);
                string fmt = _dps >= 0 ? "F" : "E";
                string fmtx = String.Format("{{0:" + fmt + "{0}}}", dps);
                string fmty = String.Format("{{0:" + fmt + "{0}}}", dps);
                string result = String.Format(fmtx, a.x) + (a.y >= 0 ? "+" : "-") + String.Format(fmty, Math.Abs(a.y)) + "i";
                result = result.Replace(',', '.');
                return result;
            }

            /****************************************************************
            prints formatted array
            ****************************************************************/
            public static string format(bool[] a)
            {
                string[] result = new string[len(a)];
                int i;
                for (i = 0; i < len(a); i++)
                    if (a[i])
                        result[i] = "true";
                    else
                        result[i] = "false";
                return "{" + String.Join(",", result) + "}";
            }

            /****************************************************************
            prints formatted array
            ****************************************************************/
            public static string format(int[] a)
            {
                string[] result = new string[len(a)];
                int i;
                for (i = 0; i < len(a); i++)
                    result[i] = a[i].ToString();
                return "{" + String.Join(",", result) + "}";
            }

            /****************************************************************
            prints formatted array
            ****************************************************************/
            public static string format(double[] a, int _dps)
            {
                int dps = Math.Abs(_dps);
                string sfmt = _dps >= 0 ? "F" : "E";
                string fmt = String.Format("{{0:" + sfmt + "{0}}}", dps);
                string[] result = new string[len(a)];
                int i;
                for (i = 0; i < len(a); i++)
                {
                    result[i] = String.Format(fmt, a[i]);
                    result[i] = result[i].Replace(',', '.');
                }
                return "{" + String.Join(",", result) + "}";
            }

            /****************************************************************
            prints formatted array
            ****************************************************************/
            public static string format(complex[] a, int _dps)
            {
                int dps = Math.Abs(_dps);
                string fmt = _dps >= 0 ? "F" : "E";
                string fmtx = String.Format("{{0:" + fmt + "{0}}}", dps);
                string fmty = String.Format("{{0:" + fmt + "{0}}}", dps);
                string[] result = new string[len(a)];
                int i;
                for (i = 0; i < len(a); i++)
                {
                    result[i] = String.Format(fmtx, a[i].x) + (a[i].y >= 0 ? "+" : "-") + String.Format(fmty, Math.Abs(a[i].y)) + "i";
                    result[i] = result[i].Replace(',', '.');
                }
                return "{" + String.Join(",", result) + "}";
            }

            /****************************************************************
            prints formatted matrix
            ****************************************************************/
            public static string format(bool[,] a)
            {
                int i, j, m, n;
                n = cols(a);
                m = rows(a);
                bool[] line = new bool[n];
                string[] result = new string[m];
                for (i = 0; i < m; i++)
                {
                    for (j = 0; j < n; j++)
                        line[j] = a[i, j];
                    result[i] = format(line);
                }
                return "{" + String.Join(",", result) + "}";
            }

            /****************************************************************
            prints formatted matrix
            ****************************************************************/
            public static string format(int[,] a)
            {
                int i, j, m, n;
                n = cols(a);
                m = rows(a);
                int[] line = new int[n];
                string[] result = new string[m];
                for (i = 0; i < m; i++)
                {
                    for (j = 0; j < n; j++)
                        line[j] = a[i, j];
                    result[i] = format(line);
                }
                return "{" + String.Join(",", result) + "}";
            }

            /****************************************************************
            prints formatted matrix
            ****************************************************************/
            public static string format(double[,] a, int dps)
            {
                int i, j, m, n;
                n = cols(a);
                m = rows(a);
                double[] line = new double[n];
                string[] result = new string[m];
                for (i = 0; i < m; i++)
                {
                    for (j = 0; j < n; j++)
                        line[j] = a[i, j];
                    result[i] = format(line, dps);
                }
                return "{" + String.Join(",", result) + "}";
            }

            /****************************************************************
            prints formatted matrix
            ****************************************************************/
            public static string format(complex[,] a, int dps)
            {
                int i, j, m, n;
                n = cols(a);
                m = rows(a);
                complex[] line = new complex[n];
                string[] result = new string[m];
                for (i = 0; i < m; i++)
                {
                    for (j = 0; j < n; j++)
                        line[j] = a[i, j];
                    result[i] = format(line, dps);
                }
                return "{" + String.Join(",", result) + "}";
            }

            /****************************************************************
            checks that matrix is symmetric.
            max|A-A^T| is calculated; if it is within 1.0E-14 of max|A|,
            matrix is considered symmetric
            ****************************************************************/
            public static bool issymmetric(double[,] a)
            {
                int i, j, n;
                double err, mx, v1, v2;
                if (rows(a) != cols(a))
                    return false;
                n = rows(a);
                if (n == 0)
                    return true;
                mx = 0;
                err = 0;
                for (i = 0; i < n; i++)
                {
                    for (j = i + 1; j < n; j++)
                    {
                        v1 = a[i, j];
                        v2 = a[j, i];
                        if (!math.isfinite(v1))
                            return false;
                        if (!math.isfinite(v2))
                            return false;
                        err = Math.Max(err, Math.Abs(v1 - v2));
                        mx = Math.Max(mx, Math.Abs(v1));
                        mx = Math.Max(mx, Math.Abs(v2));
                    }
                    v1 = a[i, i];
                    if (!math.isfinite(v1))
                        return false;
                    mx = Math.Max(mx, Math.Abs(v1));
                }
                if (mx == 0)
                    return true;
                return err / mx <= 1.0E-14;
            }

            /****************************************************************
            checks that matrix is Hermitian.
            max|A-A^H| is calculated; if it is within 1.0E-14 of max|A|,
            matrix is considered Hermitian
            ****************************************************************/
            public static bool ishermitian(complex[,] a)
            {
                int i, j, n;
                double err, mx;
                complex v1, v2, vt;
                if (rows(a) != cols(a))
                    return false;
                n = rows(a);
                if (n == 0)
                    return true;
                mx = 0;
                err = 0;
                for (i = 0; i < n; i++)
                {
                    for (j = i + 1; j < n; j++)
                    {
                        v1 = a[i, j];
                        v2 = a[j, i];
                        if (!math.isfinite(v1.x))
                            return false;
                        if (!math.isfinite(v1.y))
                            return false;
                        if (!math.isfinite(v2.x))
                            return false;
                        if (!math.isfinite(v2.y))
                            return false;
                        vt.x = v1.x - v2.x;
                        vt.y = v1.y + v2.y;
                        err = Math.Max(err, math.abscomplex(vt));
                        mx = Math.Max(mx, math.abscomplex(v1));
                        mx = Math.Max(mx, math.abscomplex(v2));
                    }
                    v1 = a[i, i];
                    if (!math.isfinite(v1.x))
                        return false;
                    if (!math.isfinite(v1.y))
                        return false;
                    double v1y = Math.Abs(v1.y);
                    err = Math.Max(err, v1y);
                    mx = Math.Max(mx, math.abscomplex(v1));
                }
                if (mx == 0)
                    return true;
                return err / mx <= 1.0E-14;
            }
            /****************************************************************
            Forces symmetricity by copying upper half of A to the lower one
            ****************************************************************/
            public static bool forcesymmetric(double[,] a)
            {
                int i, j, n;
                if (rows(a) != cols(a))
                    return false;
                n = rows(a);
                if (n == 0)
                    return true;
                for (i = 0; i < n; i++)
                    for (j = i + 1; j < n; j++)
                        a[i, j] = a[j, i];
                return true;
            }

            /****************************************************************
            Forces Hermiticity by copying upper half of A to the lower one
            ****************************************************************/
            public static bool forcehermitian(complex[,] a)
            {
                int i, j, n;
                complex v;
                if (rows(a) != cols(a))
                    return false;
                n = rows(a);
                if (n == 0)
                    return true;
                for (i = 0; i < n; i++)
                    for (j = i + 1; j < n; j++)
                    {
                        v = a[j, i];
                        a[i, j].x = v.x;
                        a[i, j].y = -v.y;
                    }
                return true;
            }
        };
        /********************************************************************
     math functions
     ********************************************************************/
        public class math
        {
            //public static System.Random RndObject = new System.Random(System.DateTime.Now.Millisecond);
            public static System.Random rndobject = new System.Random(System.DateTime.Now.Millisecond + 1000 * System.DateTime.Now.Second + 60 * 1000 * System.DateTime.Now.Minute);

            public const double machineepsilon = 5E-16;
            public const double maxrealnumber = 1E300;
            public const double minrealnumber = 1E-300;

            public static bool isfinite(double d)
            {
                return !System.Double.IsNaN(d) && !System.Double.IsInfinity(d);
            }

            public static double randomreal()
            {
                double r = 0;
                lock (rndobject) { r = rndobject.NextDouble(); }
                return r;
            }
            public static int randominteger(int N)
            {
                int r = 0;
                lock (rndobject) { r = rndobject.Next(N); }
                return r;
            }
            public static double sqr(double X)
            {
                return X * X;
            }
            public static double abscomplex(complex z)
            {
                double w;
                double xabs;
                double yabs;
                double v;

                xabs = System.Math.Abs(z.x);
                yabs = System.Math.Abs(z.y);
                w = xabs > yabs ? xabs : yabs;
                v = xabs < yabs ? xabs : yabs;
                if (v == 0)
                    return w;
                else
                {
                    double t = v / w;
                    return w * System.Math.Sqrt(1 + t * t);
                }
            }
            public static complex conj(complex z)
            {
                return new complex(z.x, -z.y);
            }
            public static complex csqr(complex z)
            {
                return new complex(z.x * z.x - z.y * z.y, 2 * z.x * z.y);
            }

        }
        /********************************************************************
     Class defining a complex number with double precision.
     ********************************************************************/
        public struct complex
        {
            public double x;
            public double y;

            public complex(double _x)
            {
                x = _x;
                y = 0;
            }
            public complex(double _x, double _y)
            {
                x = _x;
                y = _y;
            }
            public static implicit operator complex(double _x)
            {
                return new complex(_x);
            }
            public static bool operator ==(complex lhs, complex rhs)
            {
                return ((double)lhs.x == (double)rhs.x) & ((double)lhs.y == (double)rhs.y);
            }
            public static bool operator !=(complex lhs, complex rhs)
            {
                return ((double)lhs.x != (double)rhs.x) | ((double)lhs.y != (double)rhs.y);
            }
            public static complex operator +(complex lhs)
            {
                return lhs;
            }
            public static complex operator -(complex lhs)
            {
                return new complex(-lhs.x, -lhs.y);
            }
            public static complex operator +(complex lhs, complex rhs)
            {
                return new complex(lhs.x + rhs.x, lhs.y + rhs.y);
            }
            public static complex operator -(complex lhs, complex rhs)
            {
                return new complex(lhs.x - rhs.x, lhs.y - rhs.y);
            }
            public static complex operator *(complex lhs, complex rhs)
            {
                return new complex(lhs.x * rhs.x - lhs.y * rhs.y, lhs.x * rhs.y + lhs.y * rhs.x);
            }
            public static complex operator /(complex lhs, complex rhs)
            {
                complex result;
                double e;
                double f;
                if (System.Math.Abs(rhs.y) < System.Math.Abs(rhs.x))
                {
                    e = rhs.y / rhs.x;
                    f = rhs.x + rhs.y * e;
                    result.x = (lhs.x + lhs.y * e) / f;
                    result.y = (lhs.y - lhs.x * e) / f;
                }
                else
                {
                    e = rhs.x / rhs.y;
                    f = rhs.y + rhs.x * e;
                    result.x = (lhs.y + lhs.x * e) / f;
                    result.y = (-lhs.x + lhs.y * e) / f;
                }
                return result;
            }
            public override int GetHashCode()
            {
                return x.GetHashCode() ^ y.GetHashCode();
            }
            public override bool Equals(object obj)
            {
                if (obj is byte)
                    return Equals(new complex((byte)obj));
                if (obj is sbyte)
                    return Equals(new complex((sbyte)obj));
                if (obj is short)
                    return Equals(new complex((short)obj));
                if (obj is ushort)
                    return Equals(new complex((ushort)obj));
                if (obj is int)
                    return Equals(new complex((int)obj));
                if (obj is uint)
                    return Equals(new complex((uint)obj));
                if (obj is long)
                    return Equals(new complex((long)obj));
                if (obj is ulong)
                    return Equals(new complex((ulong)obj));
                if (obj is float)
                    return Equals(new complex((float)obj));
                if (obj is double)
                    return Equals(new complex((double)obj));
                if (obj is decimal)
                    return Equals(new complex((double)(decimal)obj));
                return base.Equals(obj);
            }
        }
        public class blas
        {
            public static double vectornorm2(double[] x,
                int i1,
                int i2)
            {
                double result = 0;
                int n = 0;
                int ix = 0;
                double absxi = 0;
                double scl = 0;
                double ssq = 0;

                n = i2 - i1 + 1;
                if (n < 1)
                {
                    result = 0;
                    return result;
                }
                if (n == 1)
                {
                    result = Math.Abs(x[i1]);
                    return result;
                }
                scl = 0;
                ssq = 1;
                for (ix = i1; ix <= i2; ix++)
                {
                    if ((double)(x[ix]) != (double)(0))
                    {
                        absxi = Math.Abs(x[ix]);
                        if ((double)(scl) < (double)(absxi))
                        {
                            ssq = 1 + ssq * math.sqr(scl / absxi);
                            scl = absxi;
                        }
                        else
                        {
                            ssq = ssq + math.sqr(absxi / scl);
                        }
                    }
                }
                result = scl * Math.Sqrt(ssq);
                return result;
            }


            public static int vectoridxabsmax(double[] x,
                int i1,
                int i2)
            {
                int result = 0;
                int i = 0;
                double a = 0;

                result = i1;
                a = Math.Abs(x[result]);
                for (i = i1 + 1; i <= i2; i++)
                {
                    if ((double)(Math.Abs(x[i])) > (double)(Math.Abs(x[result])))
                    {
                        result = i;
                    }
                }
                return result;
            }


            public static int columnidxabsmax(double[,] x,
                int i1,
                int i2,
                int j)
            {
                int result = 0;
                int i = 0;
                double a = 0;

                result = i1;
                a = Math.Abs(x[result, j]);
                for (i = i1 + 1; i <= i2; i++)
                {
                    if ((double)(Math.Abs(x[i, j])) > (double)(Math.Abs(x[result, j])))
                    {
                        result = i;
                    }
                }
                return result;
            }


            public static int rowidxabsmax(double[,] x,
                int j1,
                int j2,
                int i)
            {
                int result = 0;
                int j = 0;
                double a = 0;

                result = j1;
                a = Math.Abs(x[i, result]);
                for (j = j1 + 1; j <= j2; j++)
                {
                    if ((double)(Math.Abs(x[i, j])) > (double)(Math.Abs(x[i, result])))
                    {
                        result = j;
                    }
                }
                return result;
            }


            public static double upperhessenberg1norm(double[,] a,
                int i1,
                int i2,
                int j1,
                int j2,
                ref double[] work)
            {
                double result = 0;
                int i = 0;
                int j = 0;

                ap.assert(i2 - i1 == j2 - j1, "UpperHessenberg1Norm: I2-I1<>J2-J1!");
                for (j = j1; j <= j2; j++)
                {
                    work[j] = 0;
                }
                for (i = i1; i <= i2; i++)
                {
                    for (j = Math.Max(j1, j1 + i - i1 - 1); j <= j2; j++)
                    {
                        work[j] = work[j] + Math.Abs(a[i, j]);
                    }
                }
                result = 0;
                for (j = j1; j <= j2; j++)
                {
                    result = Math.Max(result, work[j]);
                }
                return result;
            }


            public static void copymatrix(double[,] a,
                int is1,
                int is2,
                int js1,
                int js2,
                ref double[,] b,
                int id1,
                int id2,
                int jd1,
                int jd2)
            {
                int isrc = 0;
                int idst = 0;
                int i_ = 0;
                int i1_ = 0;

                if (is1 > is2 | js1 > js2)
                {
                    return;
                }
                ap.assert(is2 - is1 == id2 - id1, "CopyMatrix: different sizes!");
                ap.assert(js2 - js1 == jd2 - jd1, "CopyMatrix: different sizes!");
                for (isrc = is1; isrc <= is2; isrc++)
                {
                    idst = isrc - is1 + id1;
                    i1_ = (js1) - (jd1);
                    for (i_ = jd1; i_ <= jd2; i_++)
                    {
                        b[idst, i_] = a[isrc, i_ + i1_];
                    }
                }
            }


            public static void inplacetranspose(ref double[,] a,
                int i1,
                int i2,
                int j1,
                int j2,
                ref double[] work)
            {
                int i = 0;
                int j = 0;
                int ips = 0;
                int jps = 0;
                int l = 0;
                int i_ = 0;
                int i1_ = 0;

                if (i1 > i2 | j1 > j2)
                {
                    return;
                }
                ap.assert(i1 - i2 == j1 - j2, "InplaceTranspose error: incorrect array size!");
                for (i = i1; i <= i2 - 1; i++)
                {
                    j = j1 + i - i1;
                    ips = i + 1;
                    jps = j1 + ips - i1;
                    l = i2 - i;
                    i1_ = (ips) - (1);
                    for (i_ = 1; i_ <= l; i_++)
                    {
                        work[i_] = a[i_ + i1_, j];
                    }
                    i1_ = (jps) - (ips);
                    for (i_ = ips; i_ <= i2; i_++)
                    {
                        a[i_, j] = a[i, i_ + i1_];
                    }
                    i1_ = (1) - (jps);
                    for (i_ = jps; i_ <= j2; i_++)
                    {
                        a[i, i_] = work[i_ + i1_];
                    }
                }
            }


            public static void copyandtranspose(double[,] a,
                int is1,
                int is2,
                int js1,
                int js2,
                ref double[,] b,
                int id1,
                int id2,
                int jd1,
                int jd2)
            {
                int isrc = 0;
                int jdst = 0;
                int i_ = 0;
                int i1_ = 0;

                if (is1 > is2 | js1 > js2)
                {
                    return;
                }
                ap.assert(is2 - is1 == jd2 - jd1, "CopyAndTranspose: different sizes!");
                ap.assert(js2 - js1 == id2 - id1, "CopyAndTranspose: different sizes!");
                for (isrc = is1; isrc <= is2; isrc++)
                {
                    jdst = isrc - is1 + jd1;
                    i1_ = (js1) - (id1);
                    for (i_ = id1; i_ <= id2; i_++)
                    {
                        b[i_, jdst] = a[isrc, i_ + i1_];
                    }
                }
            }


            public static void matrixvectormultiply(double[,] a,
                int i1,
                int i2,
                int j1,
                int j2,
                bool trans,
                double[] x,
                int ix1,
                int ix2,
                double alpha,
                ref double[] y,
                int iy1,
                int iy2,
                double beta)
            {
                int i = 0;
                double v = 0;
                int i_ = 0;
                int i1_ = 0;

                if (!trans)
                {

                    //
                    // y := alpha*A*x + beta*y;
                    //
                    if (i1 > i2 | j1 > j2)
                    {
                        return;
                    }
                    ap.assert(j2 - j1 == ix2 - ix1, "MatrixVectorMultiply: A and X dont match!");
                    ap.assert(i2 - i1 == iy2 - iy1, "MatrixVectorMultiply: A and Y dont match!");

                    //
                    // beta*y
                    //
                    if ((double)(beta) == (double)(0))
                    {
                        for (i = iy1; i <= iy2; i++)
                        {
                            y[i] = 0;
                        }
                    }
                    else
                    {
                        for (i_ = iy1; i_ <= iy2; i_++)
                        {
                            y[i_] = beta * y[i_];
                        }
                    }

                    //
                    // alpha*A*x
                    //
                    for (i = i1; i <= i2; i++)
                    {
                        i1_ = (ix1) - (j1);
                        v = 0.0;
                        for (i_ = j1; i_ <= j2; i_++)
                        {
                            v += a[i, i_] * x[i_ + i1_];
                        }
                        y[iy1 + i - i1] = y[iy1 + i - i1] + alpha * v;
                    }
                }
                else
                {

                    //
                    // y := alpha*A'*x + beta*y;
                    //
                    if (i1 > i2 | j1 > j2)
                    {
                        return;
                    }
                    ap.assert(i2 - i1 == ix2 - ix1, "MatrixVectorMultiply: A and X dont match!");
                    ap.assert(j2 - j1 == iy2 - iy1, "MatrixVectorMultiply: A and Y dont match!");

                    //
                    // beta*y
                    //
                    if ((double)(beta) == (double)(0))
                    {
                        for (i = iy1; i <= iy2; i++)
                        {
                            y[i] = 0;
                        }
                    }
                    else
                    {
                        for (i_ = iy1; i_ <= iy2; i_++)
                        {
                            y[i_] = beta * y[i_];
                        }
                    }

                    //
                    // alpha*A'*x
                    //
                    for (i = i1; i <= i2; i++)
                    {
                        v = alpha * x[ix1 + i - i1];
                        i1_ = (j1) - (iy1);
                        for (i_ = iy1; i_ <= iy2; i_++)
                        {
                            y[i_] = y[i_] + v * a[i, i_ + i1_];
                        }
                    }
                }
            }


            public static double pythag2(double x,
                double y)
            {
                double result = 0;
                double w = 0;
                double xabs = 0;
                double yabs = 0;
                double z = 0;

                xabs = Math.Abs(x);
                yabs = Math.Abs(y);
                w = Math.Max(xabs, yabs);
                z = Math.Min(xabs, yabs);
                if ((double)(z) == (double)(0))
                {
                    result = w;
                }
                else
                {
                    result = w * Math.Sqrt(1 + math.sqr(z / w));
                }
                return result;
            }


            public static void matrixmatrixmultiply(double[,] a,
                int ai1,
                int ai2,
                int aj1,
                int aj2,
                bool transa,
                double[,] b,
                int bi1,
                int bi2,
                int bj1,
                int bj2,
                bool transb,
                double alpha,
                ref double[,] c,
                int ci1,
                int ci2,
                int cj1,
                int cj2,
                double beta,
                ref double[] work)
            {
                int arows = 0;
                int acols = 0;
                int brows = 0;
                int bcols = 0;
                int crows = 0;
                int ccols = 0;
                int i = 0;
                int j = 0;
                int k = 0;
                int l = 0;
                int r = 0;
                double v = 0;
                int i_ = 0;
                int i1_ = 0;


                //
                // Setup
                //
                if (!transa)
                {
                    arows = ai2 - ai1 + 1;
                    acols = aj2 - aj1 + 1;
                }
                else
                {
                    arows = aj2 - aj1 + 1;
                    acols = ai2 - ai1 + 1;
                }
                if (!transb)
                {
                    brows = bi2 - bi1 + 1;
                    bcols = bj2 - bj1 + 1;
                }
                else
                {
                    brows = bj2 - bj1 + 1;
                    bcols = bi2 - bi1 + 1;
                }
                ap.assert(acols == brows, "MatrixMatrixMultiply: incorrect matrix sizes!");
                if (((arows <= 0 | acols <= 0) | brows <= 0) | bcols <= 0)
                {
                    return;
                }
                crows = arows;
                ccols = bcols;

                //
                // Test WORK
                //
                i = Math.Max(arows, acols);
                i = Math.Max(brows, i);
                i = Math.Max(i, bcols);
                work[1] = 0;
                work[i] = 0;

                //
                // Prepare C
                //
                if ((double)(beta) == (double)(0))
                {
                    for (i = ci1; i <= ci2; i++)
                    {
                        for (j = cj1; j <= cj2; j++)
                        {
                            c[i, j] = 0;
                        }
                    }
                }
                else
                {
                    for (i = ci1; i <= ci2; i++)
                    {
                        for (i_ = cj1; i_ <= cj2; i_++)
                        {
                            c[i, i_] = beta * c[i, i_];
                        }
                    }
                }

                //
                // A*B
                //
                if (!transa & !transb)
                {
                    for (l = ai1; l <= ai2; l++)
                    {
                        for (r = bi1; r <= bi2; r++)
                        {
                            v = alpha * a[l, aj1 + r - bi1];
                            k = ci1 + l - ai1;
                            i1_ = (bj1) - (cj1);
                            for (i_ = cj1; i_ <= cj2; i_++)
                            {
                                c[k, i_] = c[k, i_] + v * b[r, i_ + i1_];
                            }
                        }
                    }
                    return;
                }

                //
                // A*B'
                //
                if (!transa & transb)
                {
                    if (arows * acols < brows * bcols)
                    {
                        for (r = bi1; r <= bi2; r++)
                        {
                            for (l = ai1; l <= ai2; l++)
                            {
                                i1_ = (bj1) - (aj1);
                                v = 0.0;
                                for (i_ = aj1; i_ <= aj2; i_++)
                                {
                                    v += a[l, i_] * b[r, i_ + i1_];
                                }
                                c[ci1 + l - ai1, cj1 + r - bi1] = c[ci1 + l - ai1, cj1 + r - bi1] + alpha * v;
                            }
                        }
                        return;
                    }
                    else
                    {
                        for (l = ai1; l <= ai2; l++)
                        {
                            for (r = bi1; r <= bi2; r++)
                            {
                                i1_ = (bj1) - (aj1);
                                v = 0.0;
                                for (i_ = aj1; i_ <= aj2; i_++)
                                {
                                    v += a[l, i_] * b[r, i_ + i1_];
                                }
                                c[ci1 + l - ai1, cj1 + r - bi1] = c[ci1 + l - ai1, cj1 + r - bi1] + alpha * v;
                            }
                        }
                        return;
                    }
                }

                //
                // A'*B
                //
                if (transa & !transb)
                {
                    for (l = aj1; l <= aj2; l++)
                    {
                        for (r = bi1; r <= bi2; r++)
                        {
                            v = alpha * a[ai1 + r - bi1, l];
                            k = ci1 + l - aj1;
                            i1_ = (bj1) - (cj1);
                            for (i_ = cj1; i_ <= cj2; i_++)
                            {
                                c[k, i_] = c[k, i_] + v * b[r, i_ + i1_];
                            }
                        }
                    }
                    return;
                }

                //
                // A'*B'
                //
                if (transa & transb)
                {
                    if (arows * acols < brows * bcols)
                    {
                        for (r = bi1; r <= bi2; r++)
                        {
                            k = cj1 + r - bi1;
                            for (i = 1; i <= crows; i++)
                            {
                                work[i] = 0.0;
                            }
                            for (l = ai1; l <= ai2; l++)
                            {
                                v = alpha * b[r, bj1 + l - ai1];
                                i1_ = (aj1) - (1);
                                for (i_ = 1; i_ <= crows; i_++)
                                {
                                    work[i_] = work[i_] + v * a[l, i_ + i1_];
                                }
                            }
                            i1_ = (1) - (ci1);
                            for (i_ = ci1; i_ <= ci2; i_++)
                            {
                                c[i_, k] = c[i_, k] + work[i_ + i1_];
                            }
                        }
                        return;
                    }
                    else
                    {
                        for (l = aj1; l <= aj2; l++)
                        {
                            k = ai2 - ai1 + 1;
                            i1_ = (ai1) - (1);
                            for (i_ = 1; i_ <= k; i_++)
                            {
                                work[i_] = a[i_ + i1_, l];
                            }
                            for (r = bi1; r <= bi2; r++)
                            {
                                i1_ = (bj1) - (1);
                                v = 0.0;
                                for (i_ = 1; i_ <= k; i_++)
                                {
                                    v += work[i_] * b[r, i_ + i1_];
                                }
                                c[ci1 + l - aj1, cj1 + r - bi1] = c[ci1 + l - aj1, cj1 + r - bi1] + alpha * v;
                            }
                        }
                        return;
                    }
                }
            }


        }
        public class tsort
        {
            /*************************************************************************
            This function sorts array of real keys by ascending.

            Its results are:
            * sorted array A
            * permutation tables P1, P2

            Algorithm outputs permutation tables using two formats:
            * as usual permutation of [0..N-1]. If P1[i]=j, then sorted A[i]  contains
              value which was moved there from J-th position.
            * as a sequence of pairwise permutations. Sorted A[] may  be  obtained  by
              swaping A[i] and A[P2[i]] for all i from 0 to N-1.
          
            INPUT PARAMETERS:
                A       -   unsorted array
                N       -   array size

            OUPUT PARAMETERS:
                A       -   sorted array
                P1, P2  -   permutation tables, array[N]
            
            NOTES:
                this function assumes that A[] is finite; it doesn't checks that
                condition. All other conditions (size of input arrays, etc.) are not
                checked too.

              -- ALGLIB --
                 Copyright 14.05.2008 by Bochkanov Sergey
            *************************************************************************/
            public static void tagsort(ref double[] a,
                int n,
                ref int[] p1,
                ref int[] p2)
            {
                apserv.apbuffers buf = new apserv.apbuffers();

                p1 = new int[0];
                p2 = new int[0];

                tagsortbuf(ref a, n, ref p1, ref p2, buf);
            }


            /*************************************************************************
            Buffered variant of TagSort, which accepts preallocated output arrays as
            well as special structure for buffered allocations. If arrays are too
            short, they are reallocated. If they are large enough, no memory
            allocation is done.

            It is intended to be used in the performance-critical parts of code, where
            additional allocations can lead to severe performance degradation

              -- ALGLIB --
                 Copyright 14.05.2008 by Bochkanov Sergey
            *************************************************************************/
            public static void tagsortbuf(ref double[] a,
                int n,
                ref int[] p1,
                ref int[] p2,
                apserv.apbuffers buf)
            {
                int i = 0;
                int lv = 0;
                int lp = 0;
                int rv = 0;
                int rp = 0;


                //
                // Special cases
                //
                if (n <= 0)
                {
                    return;
                }
                if (n == 1)
                {
                    apserv.ivectorsetlengthatleast(ref p1, 1);
                    apserv.ivectorsetlengthatleast(ref p2, 1);
                    p1[0] = 0;
                    p2[0] = 0;
                    return;
                }

                //
                // General case, N>1: prepare permutations table P1
                //
                apserv.ivectorsetlengthatleast(ref p1, n);
                for (i = 0; i <= n - 1; i++)
                {
                    p1[i] = i;
                }

                //
                // General case, N>1: sort, update P1
                //
                apserv.rvectorsetlengthatleast(ref buf.ra0, n);
                apserv.ivectorsetlengthatleast(ref buf.ia0, n);
                tagsortfasti(ref a, ref p1, ref buf.ra0, ref buf.ia0, n);

                //
                // General case, N>1: fill permutations table P2
                //
                // To fill P2 we maintain two arrays:
                // * PV (Buf.IA0), Position(Value). PV[i] contains position of I-th key at the moment
                // * VP (Buf.IA1), Value(Position). VP[i] contains key which has position I at the moment
                //
                // At each step we making permutation of two items:
                //   Left, which is given by position/value pair LP/LV
                //   and Right, which is given by RP/RV
                // and updating PV[] and VP[] correspondingly.
                //
                apserv.ivectorsetlengthatleast(ref buf.ia0, n);
                apserv.ivectorsetlengthatleast(ref buf.ia1, n);
                apserv.ivectorsetlengthatleast(ref p2, n);
                for (i = 0; i <= n - 1; i++)
                {
                    buf.ia0[i] = i;
                    buf.ia1[i] = i;
                }
                for (i = 0; i <= n - 1; i++)
                {

                    //
                    // calculate LP, LV, RP, RV
                    //
                    lp = i;
                    lv = buf.ia1[lp];
                    rv = p1[i];
                    rp = buf.ia0[rv];

                    //
                    // Fill P2
                    //
                    p2[i] = rp;

                    //
                    // update PV and VP
                    //
                    buf.ia1[lp] = rv;
                    buf.ia1[rp] = lv;
                    buf.ia0[lv] = rp;
                    buf.ia0[rv] = lp;
                }
            }


            /*************************************************************************
            Same as TagSort, but optimized for real keys and integer labels.

            A is sorted, and same permutations are applied to B.

            NOTES:
            1.  this function assumes that A[] is finite; it doesn't checks that
                condition. All other conditions (size of input arrays, etc.) are not
                checked too.
            2.  this function uses two buffers, BufA and BufB, each is N elements large.
                They may be preallocated (which will save some time) or not, in which
                case function will automatically allocate memory.

              -- ALGLIB --
                 Copyright 11.12.2008 by Bochkanov Sergey
            *************************************************************************/
            public static void tagsortfasti(ref double[] a,
                ref int[] b,
                ref double[] bufa,
                ref int[] bufb,
                int n)
            {
                int i = 0;
                int j = 0;
                bool isascending = new bool();
                bool isdescending = new bool();
                double tmpr = 0;
                int tmpi = 0;


                //
                // Special case
                //
                if (n <= 1)
                {
                    return;
                }

                //
                // Test for already sorted set
                //
                isascending = true;
                isdescending = true;
                for (i = 1; i <= n - 1; i++)
                {
                    isascending = isascending & a[i] >= a[i - 1];
                    isdescending = isdescending & a[i] <= a[i - 1];
                }
                if (isascending)
                {
                    return;
                }
                if (isdescending)
                {
                    for (i = 0; i <= n - 1; i++)
                    {
                        j = n - 1 - i;
                        if (j <= i)
                        {
                            break;
                        }
                        tmpr = a[i];
                        a[i] = a[j];
                        a[j] = tmpr;
                        tmpi = b[i];
                        b[i] = b[j];
                        b[j] = tmpi;
                    }
                    return;
                }

                //
                // General case
                //
                if (ap.len(bufa) < n)
                {
                    bufa = new double[n];
                }
                if (ap.len(bufb) < n)
                {
                    bufb = new int[n];
                }
                tagsortfastirec(ref a, ref b, ref bufa, ref bufb, 0, n - 1);
            }


            /*************************************************************************
            Same as TagSort, but optimized for real keys and real labels.

            A is sorted, and same permutations are applied to B.

            NOTES:
            1.  this function assumes that A[] is finite; it doesn't checks that
                condition. All other conditions (size of input arrays, etc.) are not
                checked too.
            2.  this function uses two buffers, BufA and BufB, each is N elements large.
                They may be preallocated (which will save some time) or not, in which
                case function will automatically allocate memory.

              -- ALGLIB --
                 Copyright 11.12.2008 by Bochkanov Sergey
            *************************************************************************/
            public static void tagsortfastr(ref double[] a,
                ref double[] b,
                ref double[] bufa,
                ref double[] bufb,
                int n)
            {
                int i = 0;
                int j = 0;
                bool isascending = new bool();
                bool isdescending = new bool();
                double tmpr = 0;


                //
                // Special case
                //
                if (n <= 1)
                {
                    return;
                }

                //
                // Test for already sorted set
                //
                isascending = true;
                isdescending = true;
                for (i = 1; i <= n - 1; i++)
                {
                    isascending = isascending & a[i] >= a[i - 1];
                    isdescending = isdescending & a[i] <= a[i - 1];
                }
                if (isascending)
                {
                    return;
                }
                if (isdescending)
                {
                    for (i = 0; i <= n - 1; i++)
                    {
                        j = n - 1 - i;
                        if (j <= i)
                        {
                            break;
                        }
                        tmpr = a[i];
                        a[i] = a[j];
                        a[j] = tmpr;
                        tmpr = b[i];
                        b[i] = b[j];
                        b[j] = tmpr;
                    }
                    return;
                }

                //
                // General case
                //
                if (ap.len(bufa) < n)
                {
                    bufa = new double[n];
                }
                if (ap.len(bufb) < n)
                {
                    bufb = new double[n];
                }
                tagsortfastrrec(ref a, ref b, ref bufa, ref bufb, 0, n - 1);
            }


            /*************************************************************************
            Same as TagSort, but optimized for real keys without labels.

            A is sorted, and that's all.

            NOTES:
            1.  this function assumes that A[] is finite; it doesn't checks that
                condition. All other conditions (size of input arrays, etc.) are not
                checked too.
            2.  this function uses buffer, BufA, which is N elements large. It may be
                preallocated (which will save some time) or not, in which case
                function will automatically allocate memory.

              -- ALGLIB --
                 Copyright 11.12.2008 by Bochkanov Sergey
            *************************************************************************/
            public static void tagsortfast(ref double[] a,
                ref double[] bufa,
                int n)
            {
                int i = 0;
                int j = 0;
                bool isascending = new bool();
                bool isdescending = new bool();
                double tmpr = 0;


                //
                // Special case
                //
                if (n <= 1)
                {
                    return;
                }

                //
                // Test for already sorted set
                //
                isascending = true;
                isdescending = true;
                for (i = 1; i <= n - 1; i++)
                {
                    isascending = isascending & a[i] >= a[i - 1];
                    isdescending = isdescending & a[i] <= a[i - 1];
                }
                if (isascending)
                {
                    return;
                }
                if (isdescending)
                {
                    for (i = 0; i <= n - 1; i++)
                    {
                        j = n - 1 - i;
                        if (j <= i)
                        {
                            break;
                        }
                        tmpr = a[i];
                        a[i] = a[j];
                        a[j] = tmpr;
                    }
                    return;
                }

                //
                // General case
                //
                if (ap.len(bufa) < n)
                {
                    bufa = new double[n];
                }
                tagsortfastrec(ref a, ref bufa, 0, n - 1);
            }


            /*************************************************************************
            Heap operations: adds element to the heap

            PARAMETERS:
                A       -   heap itself, must be at least array[0..N]
                B       -   array of integer tags, which are updated according to
                            permutations in the heap
                N       -   size of the heap (without new element).
                            updated on output
                VA      -   value of the element being added
                VB      -   value of the tag

              -- ALGLIB --
                 Copyright 28.02.2010 by Bochkanov Sergey
            *************************************************************************/
            public static void tagheappushi(ref double[] a,
                ref int[] b,
                ref int n,
                double va,
                int vb)
            {
                int j = 0;
                int k = 0;
                double v = 0;

                if (n < 0)
                {
                    return;
                }

                //
                // N=0 is a special case
                //
                if (n == 0)
                {
                    a[0] = va;
                    b[0] = vb;
                    n = n + 1;
                    return;
                }

                //
                // add current point to the heap
                // (add to the bottom, then move up)
                //
                // we don't write point to the heap
                // until its final position is determined
                // (it allow us to reduce number of array access operations)
                //
                j = n;
                n = n + 1;
                while (j > 0)
                {
                    k = (j - 1) / 2;
                    v = a[k];
                    if ((double)(v) < (double)(va))
                    {

                        //
                        // swap with higher element
                        //
                        a[j] = v;
                        b[j] = b[k];
                        j = k;
                    }
                    else
                    {

                        //
                        // element in its place. terminate.
                        //
                        break;
                    }
                }
                a[j] = va;
                b[j] = vb;
            }


            /*************************************************************************
            Heap operations: replaces top element with new element
            (which is moved down)

            PARAMETERS:
                A       -   heap itself, must be at least array[0..N-1]
                B       -   array of integer tags, which are updated according to
                            permutations in the heap
                N       -   size of the heap
                VA      -   value of the element which replaces top element
                VB      -   value of the tag

              -- ALGLIB --
                 Copyright 28.02.2010 by Bochkanov Sergey
            *************************************************************************/
            public static void tagheapreplacetopi(ref double[] a,
                ref int[] b,
                int n,
                double va,
                int vb)
            {
                int j = 0;
                int k1 = 0;
                int k2 = 0;
                double v = 0;
                double v1 = 0;
                double v2 = 0;

                if (n < 1)
                {
                    return;
                }

                //
                // N=1 is a special case
                //
                if (n == 1)
                {
                    a[0] = va;
                    b[0] = vb;
                    return;
                }

                //
                // move down through heap:
                // * J  -   current element
                // * K1 -   first child (always exists)
                // * K2 -   second child (may not exists)
                //
                // we don't write point to the heap
                // until its final position is determined
                // (it allow us to reduce number of array access operations)
                //
                j = 0;
                k1 = 1;
                k2 = 2;
                while (k1 < n)
                {
                    if (k2 >= n)
                    {

                        //
                        // only one child.
                        //
                        // swap and terminate (because this child
                        // have no siblings due to heap structure)
                        //
                        v = a[k1];
                        if ((double)(v) > (double)(va))
                        {
                            a[j] = v;
                            b[j] = b[k1];
                            j = k1;
                        }
                        break;
                    }
                    else
                    {

                        //
                        // two childs
                        //
                        v1 = a[k1];
                        v2 = a[k2];
                        if ((double)(v1) > (double)(v2))
                        {
                            if ((double)(va) < (double)(v1))
                            {
                                a[j] = v1;
                                b[j] = b[k1];
                                j = k1;
                            }
                            else
                            {
                                break;
                            }
                        }
                        else
                        {
                            if ((double)(va) < (double)(v2))
                            {
                                a[j] = v2;
                                b[j] = b[k2];
                                j = k2;
                            }
                            else
                            {
                                break;
                            }
                        }
                        k1 = 2 * j + 1;
                        k2 = 2 * j + 2;
                    }
                }
                a[j] = va;
                b[j] = vb;
            }


            /*************************************************************************
            Heap operations: pops top element from the heap

            PARAMETERS:
                A       -   heap itself, must be at least array[0..N-1]
                B       -   array of integer tags, which are updated according to
                            permutations in the heap
                N       -   size of the heap, N>=1

            On output top element is moved to A[N-1], B[N-1], heap is reordered, N is
            decreased by 1.

              -- ALGLIB --
                 Copyright 28.02.2010 by Bochkanov Sergey
            *************************************************************************/
            public static void tagheappopi(ref double[] a,
                ref int[] b,
                ref int n)
            {
                double va = 0;
                int vb = 0;

                if (n < 1)
                {
                    return;
                }

                //
                // N=1 is a special case
                //
                if (n == 1)
                {
                    n = 0;
                    return;
                }

                //
                // swap top element and last element,
                // then reorder heap
                //
                va = a[n - 1];
                vb = b[n - 1];
                a[n - 1] = a[0];
                b[n - 1] = b[0];
                n = n - 1;
                tagheapreplacetopi(ref a, ref b, n, va, vb);
            }


            /*************************************************************************
            Internal TagSortFastI: sorts A[I1...I2] (both bounds are included),
            applies same permutations to B.

              -- ALGLIB --
                 Copyright 06.09.2010 by Bochkanov Sergey
            *************************************************************************/
            private static void tagsortfastirec(ref double[] a,
                ref int[] b,
                ref double[] bufa,
                ref int[] bufb,
                int i1,
                int i2)
            {
                int i = 0;
                int j = 0;
                int k = 0;
                int cntless = 0;
                int cnteq = 0;
                int cntgreater = 0;
                double tmpr = 0;
                int tmpi = 0;
                double v0 = 0;
                double v1 = 0;
                double v2 = 0;
                double vp = 0;


                //
                // Fast exit
                //
                if (i2 <= i1)
                {
                    return;
                }

                //
                // Non-recursive sort for small arrays
                //
                if (i2 - i1 <= 16)
                {
                    for (j = i1 + 1; j <= i2; j++)
                    {

                        //
                        // Search elements [I1..J-1] for place to insert Jth element.
                        //
                        // This code stops immediately if we can leave A[J] at J-th position
                        // (all elements have same value of A[J] larger than any of them)
                        //
                        tmpr = a[j];
                        tmpi = j;
                        for (k = j - 1; k >= i1; k--)
                        {
                            if (a[k] <= tmpr)
                            {
                                break;
                            }
                            tmpi = k;
                        }
                        k = tmpi;

                        //
                        // Insert Jth element into Kth position
                        //
                        if (k != j)
                        {
                            tmpr = a[j];
                            tmpi = b[j];
                            for (i = j - 1; i >= k; i--)
                            {
                                a[i + 1] = a[i];
                                b[i + 1] = b[i];
                            }
                            a[k] = tmpr;
                            b[k] = tmpi;
                        }
                    }
                    return;
                }

                //
                // Quicksort: choose pivot
                // Here we assume that I2-I1>=2
                //
                v0 = a[i1];
                v1 = a[i1 + (i2 - i1) / 2];
                v2 = a[i2];
                if (v0 > v1)
                {
                    tmpr = v1;
                    v1 = v0;
                    v0 = tmpr;
                }
                if (v1 > v2)
                {
                    tmpr = v2;
                    v2 = v1;
                    v1 = tmpr;
                }
                if (v0 > v1)
                {
                    tmpr = v1;
                    v1 = v0;
                    v0 = tmpr;
                }
                vp = v1;

                //
                // now pass through A/B and:
                // * move elements that are LESS than VP to the left of A/B
                // * move elements that are EQUAL to VP to the right of BufA/BufB (in the reverse order)
                // * move elements that are GREATER than VP to the left of BufA/BufB (in the normal order
                // * move elements from the tail of BufA/BufB to the middle of A/B (restoring normal order)
                // * move elements from the left of BufA/BufB to the end of A/B
                //
                cntless = 0;
                cnteq = 0;
                cntgreater = 0;
                for (i = i1; i <= i2; i++)
                {
                    v0 = a[i];
                    if (v0 < vp)
                    {

                        //
                        // LESS
                        //
                        k = i1 + cntless;
                        if (i != k)
                        {
                            a[k] = v0;
                            b[k] = b[i];
                        }
                        cntless = cntless + 1;
                        continue;
                    }
                    if (v0 == vp)
                    {

                        //
                        // EQUAL
                        //
                        k = i2 - cnteq;
                        bufa[k] = v0;
                        bufb[k] = b[i];
                        cnteq = cnteq + 1;
                        continue;
                    }

                    //
                    // GREATER
                    //
                    k = i1 + cntgreater;
                    bufa[k] = v0;
                    bufb[k] = b[i];
                    cntgreater = cntgreater + 1;
                }
                for (i = 0; i <= cnteq - 1; i++)
                {
                    j = i1 + cntless + cnteq - 1 - i;
                    k = i2 + i - (cnteq - 1);
                    a[j] = bufa[k];
                    b[j] = bufb[k];
                }
                for (i = 0; i <= cntgreater - 1; i++)
                {
                    j = i1 + cntless + cnteq + i;
                    k = i1 + i;
                    a[j] = bufa[k];
                    b[j] = bufb[k];
                }

                //
                // Sort left and right parts of the array (ignoring middle part)
                //
                tagsortfastirec(ref a, ref b, ref bufa, ref bufb, i1, i1 + cntless - 1);
                tagsortfastirec(ref a, ref b, ref bufa, ref bufb, i1 + cntless + cnteq, i2);
            }


            /*************************************************************************
            Internal TagSortFastR: sorts A[I1...I2] (both bounds are included),
            applies same permutations to B.

              -- ALGLIB --
                 Copyright 06.09.2010 by Bochkanov Sergey
            *************************************************************************/
            private static void tagsortfastrrec(ref double[] a,
                ref double[] b,
                ref double[] bufa,
                ref double[] bufb,
                int i1,
                int i2)
            {
                int i = 0;
                int j = 0;
                int k = 0;
                double tmpr = 0;
                double tmpr2 = 0;
                int tmpi = 0;
                int cntless = 0;
                int cnteq = 0;
                int cntgreater = 0;
                double v0 = 0;
                double v1 = 0;
                double v2 = 0;
                double vp = 0;


                //
                // Fast exit
                //
                if (i2 <= i1)
                {
                    return;
                }

                //
                // Non-recursive sort for small arrays
                //
                if (i2 - i1 <= 16)
                {
                    for (j = i1 + 1; j <= i2; j++)
                    {

                        //
                        // Search elements [I1..J-1] for place to insert Jth element.
                        //
                        // This code stops immediatly if we can leave A[J] at J-th position
                        // (all elements have same value of A[J] larger than any of them)
                        //
                        tmpr = a[j];
                        tmpi = j;
                        for (k = j - 1; k >= i1; k--)
                        {
                            if (a[k] <= tmpr)
                            {
                                break;
                            }
                            tmpi = k;
                        }
                        k = tmpi;

                        //
                        // Insert Jth element into Kth position
                        //
                        if (k != j)
                        {
                            tmpr = a[j];
                            tmpr2 = b[j];
                            for (i = j - 1; i >= k; i--)
                            {
                                a[i + 1] = a[i];
                                b[i + 1] = b[i];
                            }
                            a[k] = tmpr;
                            b[k] = tmpr2;
                        }
                    }
                    return;
                }

                //
                // Quicksort: choose pivot
                // Here we assume that I2-I1>=16
                //
                v0 = a[i1];
                v1 = a[i1 + (i2 - i1) / 2];
                v2 = a[i2];
                if (v0 > v1)
                {
                    tmpr = v1;
                    v1 = v0;
                    v0 = tmpr;
                }
                if (v1 > v2)
                {
                    tmpr = v2;
                    v2 = v1;
                    v1 = tmpr;
                }
                if (v0 > v1)
                {
                    tmpr = v1;
                    v1 = v0;
                    v0 = tmpr;
                }
                vp = v1;

                //
                // now pass through A/B and:
                // * move elements that are LESS than VP to the left of A/B
                // * move elements that are EQUAL to VP to the right of BufA/BufB (in the reverse order)
                // * move elements that are GREATER than VP to the left of BufA/BufB (in the normal order
                // * move elements from the tail of BufA/BufB to the middle of A/B (restoring normal order)
                // * move elements from the left of BufA/BufB to the end of A/B
                //
                cntless = 0;
                cnteq = 0;
                cntgreater = 0;
                for (i = i1; i <= i2; i++)
                {
                    v0 = a[i];
                    if (v0 < vp)
                    {

                        //
                        // LESS
                        //
                        k = i1 + cntless;
                        if (i != k)
                        {
                            a[k] = v0;
                            b[k] = b[i];
                        }
                        cntless = cntless + 1;
                        continue;
                    }
                    if (v0 == vp)
                    {

                        //
                        // EQUAL
                        //
                        k = i2 - cnteq;
                        bufa[k] = v0;
                        bufb[k] = b[i];
                        cnteq = cnteq + 1;
                        continue;
                    }

                    //
                    // GREATER
                    //
                    k = i1 + cntgreater;
                    bufa[k] = v0;
                    bufb[k] = b[i];
                    cntgreater = cntgreater + 1;
                }
                for (i = 0; i <= cnteq - 1; i++)
                {
                    j = i1 + cntless + cnteq - 1 - i;
                    k = i2 + i - (cnteq - 1);
                    a[j] = bufa[k];
                    b[j] = bufb[k];
                }
                for (i = 0; i <= cntgreater - 1; i++)
                {
                    j = i1 + cntless + cnteq + i;
                    k = i1 + i;
                    a[j] = bufa[k];
                    b[j] = bufb[k];
                }

                //
                // Sort left and right parts of the array (ignoring middle part)
                //
                tagsortfastrrec(ref a, ref b, ref bufa, ref bufb, i1, i1 + cntless - 1);
                tagsortfastrrec(ref a, ref b, ref bufa, ref bufb, i1 + cntless + cnteq, i2);
            }


            /*************************************************************************
            Internal TagSortFastI: sorts A[I1...I2] (both bounds are included),
            applies same permutations to B.

              -- ALGLIB --
                 Copyright 06.09.2010 by Bochkanov Sergey
            *************************************************************************/
            private static void tagsortfastrec(ref double[] a,
                ref double[] bufa,
                int i1,
                int i2)
            {
                int cntless = 0;
                int cnteq = 0;
                int cntgreater = 0;
                int i = 0;
                int j = 0;
                int k = 0;
                double tmpr = 0;
                int tmpi = 0;
                double v0 = 0;
                double v1 = 0;
                double v2 = 0;
                double vp = 0;


                //
                // Fast exit
                //
                if (i2 <= i1)
                {
                    return;
                }

                //
                // Non-recursive sort for small arrays
                //
                if (i2 - i1 <= 16)
                {
                    for (j = i1 + 1; j <= i2; j++)
                    {

                        //
                        // Search elements [I1..J-1] for place to insert Jth element.
                        //
                        // This code stops immediatly if we can leave A[J] at J-th position
                        // (all elements have same value of A[J] larger than any of them)
                        //
                        tmpr = a[j];
                        tmpi = j;
                        for (k = j - 1; k >= i1; k--)
                        {
                            if (a[k] <= tmpr)
                            {
                                break;
                            }
                            tmpi = k;
                        }
                        k = tmpi;

                        //
                        // Insert Jth element into Kth position
                        //
                        if (k != j)
                        {
                            tmpr = a[j];
                            for (i = j - 1; i >= k; i--)
                            {
                                a[i + 1] = a[i];
                            }
                            a[k] = tmpr;
                        }
                    }
                    return;
                }

                //
                // Quicksort: choose pivot
                // Here we assume that I2-I1>=16
                //
                v0 = a[i1];
                v1 = a[i1 + (i2 - i1) / 2];
                v2 = a[i2];
                if (v0 > v1)
                {
                    tmpr = v1;
                    v1 = v0;
                    v0 = tmpr;
                }
                if (v1 > v2)
                {
                    tmpr = v2;
                    v2 = v1;
                    v1 = tmpr;
                }
                if (v0 > v1)
                {
                    tmpr = v1;
                    v1 = v0;
                    v0 = tmpr;
                }
                vp = v1;

                //
                // now pass through A/B and:
                // * move elements that are LESS than VP to the left of A/B
                // * move elements that are EQUAL to VP to the right of BufA/BufB (in the reverse order)
                // * move elements that are GREATER than VP to the left of BufA/BufB (in the normal order
                // * move elements from the tail of BufA/BufB to the middle of A/B (restoring normal order)
                // * move elements from the left of BufA/BufB to the end of A/B
                //
                cntless = 0;
                cnteq = 0;
                cntgreater = 0;
                for (i = i1; i <= i2; i++)
                {
                    v0 = a[i];
                    if (v0 < vp)
                    {

                        //
                        // LESS
                        //
                        k = i1 + cntless;
                        if (i != k)
                        {
                            a[k] = v0;
                        }
                        cntless = cntless + 1;
                        continue;
                    }
                    if (v0 == vp)
                    {

                        //
                        // EQUAL
                        //
                        k = i2 - cnteq;
                        bufa[k] = v0;
                        cnteq = cnteq + 1;
                        continue;
                    }

                    //
                    // GREATER
                    //
                    k = i1 + cntgreater;
                    bufa[k] = v0;
                    cntgreater = cntgreater + 1;
                }
                for (i = 0; i <= cnteq - 1; i++)
                {
                    j = i1 + cntless + cnteq - 1 - i;
                    k = i2 + i - (cnteq - 1);
                    a[j] = bufa[k];
                }
                for (i = 0; i <= cntgreater - 1; i++)
                {
                    j = i1 + cntless + cnteq + i;
                    k = i1 + i;
                    a[j] = bufa[k];
                }

                //
                // Sort left and right parts of the array (ignoring middle part)
                //
                tagsortfastrec(ref a, ref bufa, i1, i1 + cntless - 1);
                tagsortfastrec(ref a, ref bufa, i1 + cntless + cnteq, i2);
            }


        }
        public class apserv
        {
            /*************************************************************************
            Buffers for internal functions which need buffers:
            * check for size of the buffer you want to use.
            * if buffer is too small, resize it; leave unchanged, if it is larger than
              needed.
            * use it.

            We can pass this structure to multiple functions;  after first run through
            functions buffer sizes will be finally determined,  and  on  a next run no
            allocation will be required.
            *************************************************************************/
            public class apbuffers
            {
                public int[] ia0;
                public int[] ia1;
                public int[] ia2;
                public int[] ia3;
                public double[] ra0;
                public double[] ra1;
                public double[] ra2;
                public double[] ra3;
                public apbuffers()
                {
                    ia0 = new int[0];
                    ia1 = new int[0];
                    ia2 = new int[0];
                    ia3 = new int[0];
                    ra0 = new double[0];
                    ra1 = new double[0];
                    ra2 = new double[0];
                    ra3 = new double[0];
                }
            };




            /*************************************************************************
            This  function  generates  1-dimensional  general  interpolation task with
            moderate Lipshitz constant (close to 1.0)

            If N=1 then suborutine generates only one point at the middle of [A,B]

              -- ALGLIB --
                 Copyright 02.12.2009 by Bochkanov Sergey
            *************************************************************************/
            public static void taskgenint1d(double a,
                double b,
                int n,
                ref double[] x,
                ref double[] y)
            {
                int i = 0;
                double h = 0;

                x = new double[0];
                y = new double[0];

                ap.assert(n >= 1, "TaskGenInterpolationEqdist1D: N<1!");
                x = new double[n];
                y = new double[n];
                if (n > 1)
                {
                    x[0] = a;
                    y[0] = 2 * math.randomreal() - 1;
                    h = (b - a) / (n - 1);
                    for (i = 1; i <= n - 1; i++)
                    {
                        if (i != n - 1)
                        {
                            x[i] = a + (i + 0.2 * (2 * math.randomreal() - 1)) * h;
                        }
                        else
                        {
                            x[i] = b;
                        }
                        y[i] = y[i - 1] + (2 * math.randomreal() - 1) * (x[i] - x[i - 1]);
                    }
                }
                else
                {
                    x[0] = 0.5 * (a + b);
                    y[0] = 2 * math.randomreal() - 1;
                }
            }


            /*************************************************************************
            This function generates  1-dimensional equidistant interpolation task with
            moderate Lipshitz constant (close to 1.0)

            If N=1 then suborutine generates only one point at the middle of [A,B]

              -- ALGLIB --
                 Copyright 02.12.2009 by Bochkanov Sergey
            *************************************************************************/
            public static void taskgenint1dequidist(double a,
                double b,
                int n,
                ref double[] x,
                ref double[] y)
            {
                int i = 0;
                double h = 0;

                x = new double[0];
                y = new double[0];

                ap.assert(n >= 1, "TaskGenInterpolationEqdist1D: N<1!");
                x = new double[n];
                y = new double[n];
                if (n > 1)
                {
                    x[0] = a;
                    y[0] = 2 * math.randomreal() - 1;
                    h = (b - a) / (n - 1);
                    for (i = 1; i <= n - 1; i++)
                    {
                        x[i] = a + i * h;
                        y[i] = y[i - 1] + (2 * math.randomreal() - 1) * h;
                    }
                }
                else
                {
                    x[0] = 0.5 * (a + b);
                    y[0] = 2 * math.randomreal() - 1;
                }
            }


            /*************************************************************************
            This function generates  1-dimensional Chebyshev-1 interpolation task with
            moderate Lipshitz constant (close to 1.0)

            If N=1 then suborutine generates only one point at the middle of [A,B]

              -- ALGLIB --
                 Copyright 02.12.2009 by Bochkanov Sergey
            *************************************************************************/
            public static void taskgenint1dcheb1(double a,
                double b,
                int n,
                ref double[] x,
                ref double[] y)
            {
                int i = 0;

                x = new double[0];
                y = new double[0];

                ap.assert(n >= 1, "TaskGenInterpolation1DCheb1: N<1!");
                x = new double[n];
                y = new double[n];
                if (n > 1)
                {
                    for (i = 0; i <= n - 1; i++)
                    {
                        x[i] = 0.5 * (b + a) + 0.5 * (b - a) * Math.Cos(Math.PI * (2 * i + 1) / (2 * n));
                        if (i == 0)
                        {
                            y[i] = 2 * math.randomreal() - 1;
                        }
                        else
                        {
                            y[i] = y[i - 1] + (2 * math.randomreal() - 1) * (x[i] - x[i - 1]);
                        }
                    }
                }
                else
                {
                    x[0] = 0.5 * (a + b);
                    y[0] = 2 * math.randomreal() - 1;
                }
            }


            /*************************************************************************
            This function generates  1-dimensional Chebyshev-2 interpolation task with
            moderate Lipshitz constant (close to 1.0)

            If N=1 then suborutine generates only one point at the middle of [A,B]

              -- ALGLIB --
                 Copyright 02.12.2009 by Bochkanov Sergey
            *************************************************************************/
            public static void taskgenint1dcheb2(double a,
                double b,
                int n,
                ref double[] x,
                ref double[] y)
            {
                int i = 0;

                x = new double[0];
                y = new double[0];

                ap.assert(n >= 1, "TaskGenInterpolation1DCheb2: N<1!");
                x = new double[n];
                y = new double[n];
                if (n > 1)
                {
                    for (i = 0; i <= n - 1; i++)
                    {
                        x[i] = 0.5 * (b + a) + 0.5 * (b - a) * Math.Cos(Math.PI * i / (n - 1));
                        if (i == 0)
                        {
                            y[i] = 2 * math.randomreal() - 1;
                        }
                        else
                        {
                            y[i] = y[i - 1] + (2 * math.randomreal() - 1) * (x[i] - x[i - 1]);
                        }
                    }
                }
                else
                {
                    x[0] = 0.5 * (a + b);
                    y[0] = 2 * math.randomreal() - 1;
                }
            }


            /*************************************************************************
            This function checks that all values from X[] are distinct. It does more
            than just usual floating point comparison:
            * first, it calculates max(X) and min(X)
            * second, it maps X[] from [min,max] to [1,2]
            * only at this stage actual comparison is done

            The meaning of such check is to ensure that all values are "distinct enough"
            and will not cause interpolation subroutine to fail.

            NOTE:
                X[] must be sorted by ascending (subroutine ASSERT's it)

              -- ALGLIB --
                 Copyright 02.12.2009 by Bochkanov Sergey
            *************************************************************************/
            public static bool aredistinct(double[] x,
                int n)
            {
                bool result = new bool();
                double a = 0;
                double b = 0;
                int i = 0;
                bool nonsorted = new bool();

                ap.assert(n >= 1, "APSERVAreDistinct: internal error (N<1)");
                if (n == 1)
                {

                    //
                    // everything is alright, it is up to caller to decide whether it
                    // can interpolate something with just one point
                    //
                    result = true;
                    return result;
                }
                a = x[0];
                b = x[0];
                nonsorted = false;
                for (i = 1; i <= n - 1; i++)
                {
                    a = Math.Min(a, x[i]);
                    b = Math.Max(b, x[i]);
                    nonsorted = nonsorted | (double)(x[i - 1]) >= (double)(x[i]);
                }
                ap.assert(!nonsorted, "APSERVAreDistinct: internal error (not sorted)");
                for (i = 1; i <= n - 1; i++)
                {
                    if ((double)((x[i] - a) / (b - a) + 1) == (double)((x[i - 1] - a) / (b - a) + 1))
                    {
                        result = false;
                        return result;
                    }
                }
                result = true;
                return result;
            }


            /*************************************************************************
            If Length(X)<N, resizes X

              -- ALGLIB --
                 Copyright 20.03.2009 by Bochkanov Sergey
            *************************************************************************/
            public static void bvectorsetlengthatleast(ref bool[] x,
                int n)
            {
                if (ap.len(x) < n)
                {
                    x = new bool[n];
                }
            }


            /*************************************************************************
            If Length(X)<N, resizes X

              -- ALGLIB --
                 Copyright 20.03.2009 by Bochkanov Sergey
            *************************************************************************/
            public static void ivectorsetlengthatleast(ref int[] x,
                int n)
            {
                if (ap.len(x) < n)
                {
                    x = new int[n];
                }
            }


            /*************************************************************************
            If Length(X)<N, resizes X

              -- ALGLIB --
                 Copyright 20.03.2009 by Bochkanov Sergey
            *************************************************************************/
            public static void rvectorsetlengthatleast(ref double[] x,
                int n)
            {
                if (ap.len(x) < n)
                {
                    x = new double[n];
                }
            }


            /*************************************************************************
            If Cols(X)<N or Rows(X)<M, resizes X

              -- ALGLIB --
                 Copyright 20.03.2009 by Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixsetlengthatleast(ref double[,] x,
                int m,
                int n)
            {
                if (ap.rows(x) < m | ap.cols(x) < n)
                {
                    x = new double[m, n];
                }
            }


            /*************************************************************************
            Resizes X and:
            * preserves old contents of X
            * fills new elements by zeros

              -- ALGLIB --
                 Copyright 20.03.2009 by Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixresize(ref double[,] x,
                int m,
                int n)
            {
                double[,] oldx = new double[0, 0];
                int i = 0;
                int j = 0;
                int m2 = 0;
                int n2 = 0;

                m2 = ap.rows(x);
                n2 = ap.cols(x);
                ap.swap(ref x, ref oldx);
                x = new double[m, n];
                for (i = 0; i <= m - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (i < m2 & j < n2)
                        {
                            x[i, j] = oldx[i, j];
                        }
                        else
                        {
                            x[i, j] = 0.0;
                        }
                    }
                }
            }


            /*************************************************************************
            This function checks that all values from X[] are finite

              -- ALGLIB --
                 Copyright 18.06.2010 by Bochkanov Sergey
            *************************************************************************/
            public static bool isfinitevector(double[] x,
                int n)
            {
                bool result = new bool();
                int i = 0;

                ap.assert(n >= 0, "APSERVIsFiniteVector: internal error (N<0)");
                for (i = 0; i <= n - 1; i++)
                {
                    if (!math.isfinite(x[i]))
                    {
                        result = false;
                        return result;
                    }
                }
                result = true;
                return result;
            }


            /*************************************************************************
            This function checks that all values from X[] are finite

              -- ALGLIB --
                 Copyright 18.06.2010 by Bochkanov Sergey
            *************************************************************************/
            public static bool isfinitecvector(complex[] z,
                int n)
            {
                bool result = new bool();
                int i = 0;

                ap.assert(n >= 0, "APSERVIsFiniteCVector: internal error (N<0)");
                for (i = 0; i <= n - 1; i++)
                {
                    if (!math.isfinite(z[i].x) | !math.isfinite(z[i].y))
                    {
                        result = false;
                        return result;
                    }
                }
                result = true;
                return result;
            }


            /*************************************************************************
            This function checks that all values from X[0..M-1,0..N-1] are finite

              -- ALGLIB --
                 Copyright 18.06.2010 by Bochkanov Sergey
            *************************************************************************/
            public static bool apservisfinitematrix(double[,] x,
                int m,
                int n)
            {
                bool result = new bool();
                int i = 0;
                int j = 0;

                ap.assert(n >= 0, "APSERVIsFiniteMatrix: internal error (N<0)");
                ap.assert(m >= 0, "APSERVIsFiniteMatrix: internal error (M<0)");
                for (i = 0; i <= m - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (!math.isfinite(x[i, j]))
                        {
                            result = false;
                            return result;
                        }
                    }
                }
                result = true;
                return result;
            }


            /*************************************************************************
            This function checks that all values from X[0..M-1,0..N-1] are finite

              -- ALGLIB --
                 Copyright 18.06.2010 by Bochkanov Sergey
            *************************************************************************/
            public static bool apservisfinitecmatrix(complex[,] x,
                int m,
                int n)
            {
                bool result = new bool();
                int i = 0;
                int j = 0;

                ap.assert(n >= 0, "APSERVIsFiniteCMatrix: internal error (N<0)");
                ap.assert(m >= 0, "APSERVIsFiniteCMatrix: internal error (M<0)");
                for (i = 0; i <= m - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (!math.isfinite(x[i, j].x) | !math.isfinite(x[i, j].y))
                        {
                            result = false;
                            return result;
                        }
                    }
                }
                result = true;
                return result;
            }


            /*************************************************************************
            This function checks that all values from upper/lower triangle of
            X[0..N-1,0..N-1] are finite

              -- ALGLIB --
                 Copyright 18.06.2010 by Bochkanov Sergey
            *************************************************************************/
            public static bool isfinitertrmatrix(double[,] x,
                int n,
                bool isupper)
            {
                bool result = new bool();
                int i = 0;
                int j1 = 0;
                int j2 = 0;
                int j = 0;

                ap.assert(n >= 0, "APSERVIsFiniteRTRMatrix: internal error (N<0)");
                for (i = 0; i <= n - 1; i++)
                {
                    if (isupper)
                    {
                        j1 = i;
                        j2 = n - 1;
                    }
                    else
                    {
                        j1 = 0;
                        j2 = i;
                    }
                    for (j = j1; j <= j2; j++)
                    {
                        if (!math.isfinite(x[i, j]))
                        {
                            result = false;
                            return result;
                        }
                    }
                }
                result = true;
                return result;
            }


            /*************************************************************************
            This function checks that all values from upper/lower triangle of
            X[0..N-1,0..N-1] are finite

              -- ALGLIB --
                 Copyright 18.06.2010 by Bochkanov Sergey
            *************************************************************************/
            public static bool apservisfinitectrmatrix(complex[,] x,
                int n,
                bool isupper)
            {
                bool result = new bool();
                int i = 0;
                int j1 = 0;
                int j2 = 0;
                int j = 0;

                ap.assert(n >= 0, "APSERVIsFiniteCTRMatrix: internal error (N<0)");
                for (i = 0; i <= n - 1; i++)
                {
                    if (isupper)
                    {
                        j1 = i;
                        j2 = n - 1;
                    }
                    else
                    {
                        j1 = 0;
                        j2 = i;
                    }
                    for (j = j1; j <= j2; j++)
                    {
                        if (!math.isfinite(x[i, j].x) | !math.isfinite(x[i, j].y))
                        {
                            result = false;
                            return result;
                        }
                    }
                }
                result = true;
                return result;
            }


            /*************************************************************************
            This function checks that all values from X[0..M-1,0..N-1] are  finite  or
            NaN's.

              -- ALGLIB --
                 Copyright 18.06.2010 by Bochkanov Sergey
            *************************************************************************/
            public static bool apservisfiniteornanmatrix(double[,] x,
                int m,
                int n)
            {
                bool result = new bool();
                int i = 0;
                int j = 0;

                ap.assert(n >= 0, "APSERVIsFiniteOrNaNMatrix: internal error (N<0)");
                ap.assert(m >= 0, "APSERVIsFiniteOrNaNMatrix: internal error (M<0)");
                for (i = 0; i <= m - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (!(math.isfinite(x[i, j]) | Double.IsNaN(x[i, j])))
                        {
                            result = false;
                            return result;
                        }
                    }
                }
                result = true;
                return result;
            }


            /*************************************************************************
            Safe sqrt(x^2+y^2)

              -- ALGLIB --
                 Copyright by Bochkanov Sergey
            *************************************************************************/
            public static double safepythag2(double x,
                double y)
            {
                double result = 0;
                double w = 0;
                double xabs = 0;
                double yabs = 0;
                double z = 0;

                xabs = Math.Abs(x);
                yabs = Math.Abs(y);
                w = Math.Max(xabs, yabs);
                z = Math.Min(xabs, yabs);
                if ((double)(z) == (double)(0))
                {
                    result = w;
                }
                else
                {
                    result = w * Math.Sqrt(1 + math.sqr(z / w));
                }
                return result;
            }


            /*************************************************************************
            Safe sqrt(x^2+y^2)

              -- ALGLIB --
                 Copyright by Bochkanov Sergey
            *************************************************************************/
            public static double safepythag3(double x,
                double y,
                double z)
            {
                double result = 0;
                double w = 0;

                w = Math.Max(Math.Abs(x), Math.Max(Math.Abs(y), Math.Abs(z)));
                if ((double)(w) == (double)(0))
                {
                    result = 0;
                    return result;
                }
                x = x / w;
                y = y / w;
                z = z / w;
                result = w * Math.Sqrt(math.sqr(x) + math.sqr(y) + math.sqr(z));
                return result;
            }


            /*************************************************************************
            Safe division.

            This function attempts to calculate R=X/Y without overflow.

            It returns:
            * +1, if abs(X/Y)>=MaxRealNumber or undefined - overflow-like situation
                  (no overlfow is generated, R is either NAN, PosINF, NegINF)
            *  0, if MinRealNumber<abs(X/Y)<MaxRealNumber or X=0, Y<>0
                  (R contains result, may be zero)
            * -1, if 0<abs(X/Y)<MinRealNumber - underflow-like situation
                  (R contains zero; it corresponds to underflow)

            No overflow is generated in any case.

              -- ALGLIB --
                 Copyright by Bochkanov Sergey
            *************************************************************************/
            public static int saferdiv(double x,
                double y,
                ref double r)
            {
                int result = 0;

                r = 0;


                //
                // Two special cases:
                // * Y=0
                // * X=0 and Y<>0
                //
                if ((double)(y) == (double)(0))
                {
                    result = 1;
                    if ((double)(x) == (double)(0))
                    {
                        r = Double.NaN;
                    }
                    if ((double)(x) > (double)(0))
                    {
                        r = Double.PositiveInfinity;
                    }
                    if ((double)(x) < (double)(0))
                    {
                        r = Double.NegativeInfinity;
                    }
                    return result;
                }
                if ((double)(x) == (double)(0))
                {
                    r = 0;
                    result = 0;
                    return result;
                }

                //
                // make Y>0
                //
                if ((double)(y) < (double)(0))
                {
                    x = -x;
                    y = -y;
                }

                //
                //
                //
                if ((double)(y) >= (double)(1))
                {
                    r = x / y;
                    if ((double)(Math.Abs(r)) <= (double)(math.minrealnumber))
                    {
                        result = -1;
                        r = 0;
                    }
                    else
                    {
                        result = 0;
                    }
                }
                else
                {
                    if ((double)(Math.Abs(x)) >= (double)(math.maxrealnumber * y))
                    {
                        if ((double)(x) > (double)(0))
                        {
                            r = Double.PositiveInfinity;
                        }
                        else
                        {
                            r = Double.NegativeInfinity;
                        }
                        result = 1;
                    }
                    else
                    {
                        r = x / y;
                        result = 0;
                    }
                }
                return result;
            }


            /*************************************************************************
            This function calculates "safe" min(X/Y,V) for positive finite X, Y, V.
            No overflow is generated in any case.

              -- ALGLIB --
                 Copyright by Bochkanov Sergey
            *************************************************************************/
            public static double safeminposrv(double x,
                double y,
                double v)
            {
                double result = 0;
                double r = 0;

                if ((double)(y) >= (double)(1))
                {

                    //
                    // Y>=1, we can safely divide by Y
                    //
                    r = x / y;
                    result = v;
                    if ((double)(v) > (double)(r))
                    {
                        result = r;
                    }
                    else
                    {
                        result = v;
                    }
                }
                else
                {

                    //
                    // Y<1, we can safely multiply by Y
                    //
                    if ((double)(x) < (double)(v * y))
                    {
                        result = x / y;
                    }
                    else
                    {
                        result = v;
                    }
                }
                return result;
            }


            /*************************************************************************
            This function makes periodic mapping of X to [A,B].

            It accepts X, A, B (A>B). It returns T which lies in  [A,B] and integer K,
            such that X = T + K*(B-A).

            NOTES:
            * K is represented as real value, although actually it is integer
            * T is guaranteed to be in [A,B]
            * T replaces X

              -- ALGLIB --
                 Copyright by Bochkanov Sergey
            *************************************************************************/
            public static void apperiodicmap(ref double x,
                double a,
                double b,
                ref double k)
            {
                k = 0;

                ap.assert((double)(a) < (double)(b), "APPeriodicMap: internal error!");
                k = (int)Math.Floor((x - a) / (b - a));
                x = x - k * (b - a);
                while ((double)(x) < (double)(a))
                {
                    x = x + (b - a);
                    k = k - 1;
                }
                while ((double)(x) > (double)(b))
                {
                    x = x - (b - a);
                    k = k + 1;
                }
                x = Math.Max(x, a);
                x = Math.Min(x, b);
            }


            /*************************************************************************
            'bounds' value: maps X to [B1,B2]

              -- ALGLIB --
                 Copyright 20.03.2009 by Bochkanov Sergey
            *************************************************************************/
            public static double boundval(double x,
                double b1,
                double b2)
            {
                double result = 0;

                if ((double)(x) <= (double)(b1))
                {
                    result = b1;
                    return result;
                }
                if ((double)(x) >= (double)(b2))
                {
                    result = b2;
                    return result;
                }
                result = x;
                return result;
            }


            /*************************************************************************
            Allocation of serializer: complex value
            *************************************************************************/
            public static void alloccomplex(serializer s,
                complex v)
            {
                s.alloc_entry();
                s.alloc_entry();
            }


            /*************************************************************************
            Serialization: complex value
            *************************************************************************/
            public static void serializecomplex(serializer s,
                complex v)
            {
                s.serialize_double(v.x);
                s.serialize_double(v.y);
            }


            /*************************************************************************
            Unserialization: complex value
            *************************************************************************/
            public static complex unserializecomplex(serializer s)
            {
                complex result = 0;

                result.x = s.unserialize_double();
                result.y = s.unserialize_double();
                return result;
            }


            /*************************************************************************
            Allocation of serializer: real array
            *************************************************************************/
            public static void allocrealarray(serializer s,
                double[] v,
                int n)
            {
                int i = 0;

                if (n < 0)
                {
                    n = ap.len(v);
                }
                s.alloc_entry();
                for (i = 0; i <= n - 1; i++)
                {
                    s.alloc_entry();
                }
            }


            /*************************************************************************
            Serialization: complex value
            *************************************************************************/
            public static void serializerealarray(serializer s,
                double[] v,
                int n)
            {
                int i = 0;

                if (n < 0)
                {
                    n = ap.len(v);
                }
                s.serialize_int(n);
                for (i = 0; i <= n - 1; i++)
                {
                    s.serialize_double(v[i]);
                }
            }


            /*************************************************************************
            Unserialization: complex value
            *************************************************************************/
            public static void unserializerealarray(serializer s,
                ref double[] v)
            {
                int n = 0;
                int i = 0;
                double t = 0;

                v = new double[0];

                n = s.unserialize_int();
                if (n == 0)
                {
                    return;
                }
                v = new double[n];
                for (i = 0; i <= n - 1; i++)
                {
                    t = s.unserialize_double();
                    v[i] = t;
                }
            }


            /*************************************************************************
            Allocation of serializer: Integer array
            *************************************************************************/
            public static void allocintegerarray(serializer s,
                int[] v,
                int n)
            {
                int i = 0;

                if (n < 0)
                {
                    n = ap.len(v);
                }
                s.alloc_entry();
                for (i = 0; i <= n - 1; i++)
                {
                    s.alloc_entry();
                }
            }


            /*************************************************************************
            Serialization: Integer array
            *************************************************************************/
            public static void serializeintegerarray(serializer s,
                int[] v,
                int n)
            {
                int i = 0;

                if (n < 0)
                {
                    n = ap.len(v);
                }
                s.serialize_int(n);
                for (i = 0; i <= n - 1; i++)
                {
                    s.serialize_int(v[i]);
                }
            }


            /*************************************************************************
            Unserialization: complex value
            *************************************************************************/
            public static void unserializeintegerarray(serializer s,
                ref int[] v)
            {
                int n = 0;
                int i = 0;
                int t = 0;

                v = new int[0];

                n = s.unserialize_int();
                if (n == 0)
                {
                    return;
                }
                v = new int[n];
                for (i = 0; i <= n - 1; i++)
                {
                    t = s.unserialize_int();
                    v[i] = t;
                }
            }


            /*************************************************************************
            Allocation of serializer: real matrix
            *************************************************************************/
            public static void allocrealmatrix(serializer s,
                double[,] v,
                int n0,
                int n1)
            {
                int i = 0;
                int j = 0;

                if (n0 < 0)
                {
                    n0 = ap.rows(v);
                }
                if (n1 < 0)
                {
                    n1 = ap.cols(v);
                }
                s.alloc_entry();
                s.alloc_entry();
                for (i = 0; i <= n0 - 1; i++)
                {
                    for (j = 0; j <= n1 - 1; j++)
                    {
                        s.alloc_entry();
                    }
                }
            }


            /*************************************************************************
            Serialization: complex value
            *************************************************************************/
            public static void serializerealmatrix(serializer s,
                double[,] v,
                int n0,
                int n1)
            {
                int i = 0;
                int j = 0;

                if (n0 < 0)
                {
                    n0 = ap.rows(v);
                }
                if (n1 < 0)
                {
                    n1 = ap.cols(v);
                }
                s.serialize_int(n0);
                s.serialize_int(n1);
                for (i = 0; i <= n0 - 1; i++)
                {
                    for (j = 0; j <= n1 - 1; j++)
                    {
                        s.serialize_double(v[i, j]);
                    }
                }
            }


            /*************************************************************************
            Unserialization: complex value
            *************************************************************************/
            public static void unserializerealmatrix(serializer s,
                ref double[,] v)
            {
                int i = 0;
                int j = 0;
                int n0 = 0;
                int n1 = 0;
                double t = 0;

                v = new double[0, 0];

                n0 = s.unserialize_int();
                n1 = s.unserialize_int();
                if (n0 == 0 | n1 == 0)
                {
                    return;
                }
                v = new double[n0, n1];
                for (i = 0; i <= n0 - 1; i++)
                {
                    for (j = 0; j <= n1 - 1; j++)
                    {
                        t = s.unserialize_double();
                        v[i, j] = t;
                    }
                }
            }


            /*************************************************************************
            Copy integer array
            *************************************************************************/
            public static void copyintegerarray(int[] src,
                ref int[] dst)
            {
                int i = 0;

                dst = new int[0];

                if (ap.len(src) > 0)
                {
                    dst = new int[ap.len(src)];
                    for (i = 0; i <= ap.len(src) - 1; i++)
                    {
                        dst[i] = src[i];
                    }
                }
            }


            /*************************************************************************
            Copy real array
            *************************************************************************/
            public static void copyrealarray(double[] src,
                ref double[] dst)
            {
                int i = 0;

                dst = new double[0];

                if (ap.len(src) > 0)
                {
                    dst = new double[ap.len(src)];
                    for (i = 0; i <= ap.len(src) - 1; i++)
                    {
                        dst[i] = src[i];
                    }
                }
            }


            /*************************************************************************
            Copy real matrix
            *************************************************************************/
            public static void copyrealmatrix(double[,] src,
                ref double[,] dst)
            {
                int i = 0;
                int j = 0;

                dst = new double[0, 0];

                if (ap.rows(src) > 0 & ap.cols(src) > 0)
                {
                    dst = new double[ap.rows(src), ap.cols(src)];
                    for (i = 0; i <= ap.rows(src) - 1; i++)
                    {
                        for (j = 0; j <= ap.cols(src) - 1; j++)
                        {
                            dst[i, j] = src[i, j];
                        }
                    }
                }
            }


            /*************************************************************************
            This function searches integer array. Elements in this array are actually
            records, each NRec elements wide. Each record has unique header - NHeader
            integer values, which identify it. Records are lexicographically sorted by
            header.

            Records are identified by their index, not offset (offset = NRec*index).

            This function searches A (records with indices [I0,I1)) for a record with
            header B. It returns index of this record (not offset!), or -1 on failure.

              -- ALGLIB --
                 Copyright 28.03.2011 by Bochkanov Sergey
            *************************************************************************/
            public static int recsearch(ref int[] a,
                int nrec,
                int nheader,
                int i0,
                int i1,
                int[] b)
            {
                int result = 0;
                int mididx = 0;
                int cflag = 0;
                int k = 0;
                int offs = 0;

                result = -1;
                while (true)
                {
                    if (i0 >= i1)
                    {
                        break;
                    }
                    mididx = (i0 + i1) / 2;
                    offs = nrec * mididx;
                    cflag = 0;
                    for (k = 0; k <= nheader - 1; k++)
                    {
                        if (a[offs + k] < b[k])
                        {
                            cflag = -1;
                            break;
                        }
                        if (a[offs + k] > b[k])
                        {
                            cflag = 1;
                            break;
                        }
                    }
                    if (cflag == 0)
                    {
                        result = mididx;
                        return result;
                    }
                    if (cflag < 0)
                    {
                        i0 = mididx + 1;
                    }
                    else
                    {
                        i1 = mididx;
                    }
                }
                return result;
            }


        }
        public class ablas
        {
            /*************************************************************************
            Splits matrix length in two parts, left part should match ABLAS block size

            INPUT PARAMETERS
                A   -   real matrix, is passed to ensure that we didn't split
                        complex matrix using real splitting subroutine.
                        matrix itself is not changed.
                N   -   length, N>0

            OUTPUT PARAMETERS
                N1  -   length
                N2  -   length

            N1+N2=N, N1>=N2, N2 may be zero

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void ablassplitlength(double[,] a,
                int n,
                ref int n1,
                ref int n2)
            {
                n1 = 0;
                n2 = 0;

                if (n > ablasblocksize(a))
                {
                    ablasinternalsplitlength(n, ablasblocksize(a), ref n1, ref n2);
                }
                else
                {
                    ablasinternalsplitlength(n, ablasmicroblocksize(), ref n1, ref n2);
                }
            }


            /*************************************************************************
            Complex ABLASSplitLength

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void ablascomplexsplitlength(complex[,] a,
                int n,
                ref int n1,
                ref int n2)
            {
                n1 = 0;
                n2 = 0;

                if (n > ablascomplexblocksize(a))
                {
                    ablasinternalsplitlength(n, ablascomplexblocksize(a), ref n1, ref n2);
                }
                else
                {
                    ablasinternalsplitlength(n, ablasmicroblocksize(), ref n1, ref n2);
                }
            }


            /*************************************************************************
            Returns block size - subdivision size where  cache-oblivious  soubroutines
            switch to the optimized kernel.

            INPUT PARAMETERS
                A   -   real matrix, is passed to ensure that we didn't split
                        complex matrix using real splitting subroutine.
                        matrix itself is not changed.

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static int ablasblocksize(double[,] a)
            {
                int result = 0;

                result = 32;
                return result;
            }


            /*************************************************************************
            Block size for complex subroutines.

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static int ablascomplexblocksize(complex[,] a)
            {
                int result = 0;

                result = 24;
                return result;
            }


            /*************************************************************************
            Microblock size

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static int ablasmicroblocksize()
            {
                int result = 0;

                result = 8;
                return result;
            }


            /*************************************************************************
            Cache-oblivous complex "copy-and-transpose"

            Input parameters:
                M   -   number of rows
                N   -   number of columns
                A   -   source matrix, MxN submatrix is copied and transposed
                IA  -   submatrix offset (row index)
                JA  -   submatrix offset (column index)
                A   -   destination matrix
                IB  -   submatrix offset (row index)
                JB  -   submatrix offset (column index)
            *************************************************************************/
            public static void cmatrixtranspose(int m,
                int n,
                complex[,] a,
                int ia,
                int ja,
                ref complex[,] b,
                int ib,
                int jb)
            {
                int i = 0;
                int s1 = 0;
                int s2 = 0;
                int i_ = 0;
                int i1_ = 0;

                if (m <= 2 * ablascomplexblocksize(a) & n <= 2 * ablascomplexblocksize(a))
                {

                    //
                    // base case
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        i1_ = (ja) - (ib);
                        for (i_ = ib; i_ <= ib + n - 1; i_++)
                        {
                            b[i_, jb + i] = a[ia + i, i_ + i1_];
                        }
                    }
                }
                else
                {

                    //
                    // Cache-oblivious recursion
                    //
                    if (m > n)
                    {
                        ablascomplexsplitlength(a, m, ref s1, ref s2);
                        cmatrixtranspose(s1, n, a, ia, ja, ref b, ib, jb);
                        cmatrixtranspose(s2, n, a, ia + s1, ja, ref b, ib, jb + s1);
                    }
                    else
                    {
                        ablascomplexsplitlength(a, n, ref s1, ref s2);
                        cmatrixtranspose(m, s1, a, ia, ja, ref b, ib, jb);
                        cmatrixtranspose(m, s2, a, ia, ja + s1, ref b, ib + s1, jb);
                    }
                }
            }


            /*************************************************************************
            Cache-oblivous real "copy-and-transpose"

            Input parameters:
                M   -   number of rows
                N   -   number of columns
                A   -   source matrix, MxN submatrix is copied and transposed
                IA  -   submatrix offset (row index)
                JA  -   submatrix offset (column index)
                A   -   destination matrix
                IB  -   submatrix offset (row index)
                JB  -   submatrix offset (column index)
            *************************************************************************/
            public static void rmatrixtranspose(int m,
                int n,
                double[,] a,
                int ia,
                int ja,
                ref double[,] b,
                int ib,
                int jb)
            {
                int i = 0;
                int s1 = 0;
                int s2 = 0;
                int i_ = 0;
                int i1_ = 0;

                if (m <= 2 * ablasblocksize(a) & n <= 2 * ablasblocksize(a))
                {

                    //
                    // base case
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        i1_ = (ja) - (ib);
                        for (i_ = ib; i_ <= ib + n - 1; i_++)
                        {
                            b[i_, jb + i] = a[ia + i, i_ + i1_];
                        }
                    }
                }
                else
                {

                    //
                    // Cache-oblivious recursion
                    //
                    if (m > n)
                    {
                        ablassplitlength(a, m, ref s1, ref s2);
                        rmatrixtranspose(s1, n, a, ia, ja, ref b, ib, jb);
                        rmatrixtranspose(s2, n, a, ia + s1, ja, ref b, ib, jb + s1);
                    }
                    else
                    {
                        ablassplitlength(a, n, ref s1, ref s2);
                        rmatrixtranspose(m, s1, a, ia, ja, ref b, ib, jb);
                        rmatrixtranspose(m, s2, a, ia, ja + s1, ref b, ib + s1, jb);
                    }
                }
            }


            /*************************************************************************
            Copy

            Input parameters:
                M   -   number of rows
                N   -   number of columns
                A   -   source matrix, MxN submatrix is copied and transposed
                IA  -   submatrix offset (row index)
                JA  -   submatrix offset (column index)
                B   -   destination matrix
                IB  -   submatrix offset (row index)
                JB  -   submatrix offset (column index)
            *************************************************************************/
            public static void cmatrixcopy(int m,
                int n,
                complex[,] a,
                int ia,
                int ja,
                ref complex[,] b,
                int ib,
                int jb)
            {
                int i = 0;
                int i_ = 0;
                int i1_ = 0;

                for (i = 0; i <= m - 1; i++)
                {
                    i1_ = (ja) - (jb);
                    for (i_ = jb; i_ <= jb + n - 1; i_++)
                    {
                        b[ib + i, i_] = a[ia + i, i_ + i1_];
                    }
                }
            }


            /*************************************************************************
            Copy

            Input parameters:
                M   -   number of rows
                N   -   number of columns
                A   -   source matrix, MxN submatrix is copied and transposed
                IA  -   submatrix offset (row index)
                JA  -   submatrix offset (column index)
                B   -   destination matrix
                IB  -   submatrix offset (row index)
                JB  -   submatrix offset (column index)
            *************************************************************************/
            public static void rmatrixcopy(int m,
                int n,
                double[,] a,
                int ia,
                int ja,
                ref double[,] b,
                int ib,
                int jb)
            {
                int i = 0;
                int i_ = 0;
                int i1_ = 0;

                for (i = 0; i <= m - 1; i++)
                {
                    i1_ = (ja) - (jb);
                    for (i_ = jb; i_ <= jb + n - 1; i_++)
                    {
                        b[ib + i, i_] = a[ia + i, i_ + i1_];
                    }
                }
            }


            /*************************************************************************
            Rank-1 correction: A := A + u*v'

            INPUT PARAMETERS:
                M   -   number of rows
                N   -   number of columns
                A   -   target matrix, MxN submatrix is updated
                IA  -   submatrix offset (row index)
                JA  -   submatrix offset (column index)
                U   -   vector #1
                IU  -   subvector offset
                V   -   vector #2
                IV  -   subvector offset
            *************************************************************************/
            public static void cmatrixrank1(int m,
                int n,
                ref complex[,] a,
                int ia,
                int ja,
                ref complex[] u,
                int iu,
                ref complex[] v,
                int iv)
            {
                int i = 0;
                complex s = 0;
                int i_ = 0;
                int i1_ = 0;

                if (m == 0 | n == 0)
                {
                    return;
                }
                if (ablasf.cmatrixrank1f(m, n, ref a, ia, ja, ref u, iu, ref v, iv))
                {
                    return;
                }
                for (i = 0; i <= m - 1; i++)
                {
                    s = u[iu + i];
                    i1_ = (iv) - (ja);
                    for (i_ = ja; i_ <= ja + n - 1; i_++)
                    {
                        a[ia + i, i_] = a[ia + i, i_] + s * v[i_ + i1_];
                    }
                }
            }


            /*************************************************************************
            Rank-1 correction: A := A + u*v'

            INPUT PARAMETERS:
                M   -   number of rows
                N   -   number of columns
                A   -   target matrix, MxN submatrix is updated
                IA  -   submatrix offset (row index)
                JA  -   submatrix offset (column index)
                U   -   vector #1
                IU  -   subvector offset
                V   -   vector #2
                IV  -   subvector offset
            *************************************************************************/
            public static void rmatrixrank1(int m,
                int n,
                ref double[,] a,
                int ia,
                int ja,
                ref double[] u,
                int iu,
                ref double[] v,
                int iv)
            {
                int i = 0;
                double s = 0;
                int i_ = 0;
                int i1_ = 0;

                if (m == 0 | n == 0)
                {
                    return;
                }
                if (ablasf.rmatrixrank1f(m, n, ref a, ia, ja, ref u, iu, ref v, iv))
                {
                    return;
                }
                for (i = 0; i <= m - 1; i++)
                {
                    s = u[iu + i];
                    i1_ = (iv) - (ja);
                    for (i_ = ja; i_ <= ja + n - 1; i_++)
                    {
                        a[ia + i, i_] = a[ia + i, i_] + s * v[i_ + i1_];
                    }
                }
            }


            /*************************************************************************
            Matrix-vector product: y := op(A)*x

            INPUT PARAMETERS:
                M   -   number of rows of op(A)
                        M>=0
                N   -   number of columns of op(A)
                        N>=0
                A   -   target matrix
                IA  -   submatrix offset (row index)
                JA  -   submatrix offset (column index)
                OpA -   operation type:
                        * OpA=0     =>  op(A) = A
                        * OpA=1     =>  op(A) = A^T
                        * OpA=2     =>  op(A) = A^H
                X   -   input vector
                IX  -   subvector offset
                IY  -   subvector offset

            OUTPUT PARAMETERS:
                Y   -   vector which stores result

            if M=0, then subroutine does nothing.
            if N=0, Y is filled by zeros.


              -- ALGLIB routine --

                 28.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void cmatrixmv(int m,
                int n,
                complex[,] a,
                int ia,
                int ja,
                int opa,
                complex[] x,
                int ix,
                ref complex[] y,
                int iy)
            {
                int i = 0;
                complex v = 0;
                int i_ = 0;
                int i1_ = 0;

                if (m == 0)
                {
                    return;
                }
                if (n == 0)
                {
                    for (i = 0; i <= m - 1; i++)
                    {
                        y[iy + i] = 0;
                    }
                    return;
                }
                if (ablasf.cmatrixmvf(m, n, a, ia, ja, opa, x, ix, ref y, iy))
                {
                    return;
                }
                if (opa == 0)
                {

                    //
                    // y = A*x
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        i1_ = (ix) - (ja);
                        v = 0.0;
                        for (i_ = ja; i_ <= ja + n - 1; i_++)
                        {
                            v += a[ia + i, i_] * x[i_ + i1_];
                        }
                        y[iy + i] = v;
                    }
                    return;
                }
                if (opa == 1)
                {

                    //
                    // y = A^T*x
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        y[iy + i] = 0;
                    }
                    for (i = 0; i <= n - 1; i++)
                    {
                        v = x[ix + i];
                        i1_ = (ja) - (iy);
                        for (i_ = iy; i_ <= iy + m - 1; i_++)
                        {
                            y[i_] = y[i_] + v * a[ia + i, i_ + i1_];
                        }
                    }
                    return;
                }
                if (opa == 2)
                {

                    //
                    // y = A^H*x
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        y[iy + i] = 0;
                    }
                    for (i = 0; i <= n - 1; i++)
                    {
                        v = x[ix + i];
                        i1_ = (ja) - (iy);
                        for (i_ = iy; i_ <= iy + m - 1; i_++)
                        {
                            y[i_] = y[i_] + v * math.conj(a[ia + i, i_ + i1_]);
                        }
                    }
                    return;
                }
            }


            /*************************************************************************
            Matrix-vector product: y := op(A)*x

            INPUT PARAMETERS:
                M   -   number of rows of op(A)
                N   -   number of columns of op(A)
                A   -   target matrix
                IA  -   submatrix offset (row index)
                JA  -   submatrix offset (column index)
                OpA -   operation type:
                        * OpA=0     =>  op(A) = A
                        * OpA=1     =>  op(A) = A^T
                X   -   input vector
                IX  -   subvector offset
                IY  -   subvector offset

            OUTPUT PARAMETERS:
                Y   -   vector which stores result

            if M=0, then subroutine does nothing.
            if N=0, Y is filled by zeros.


              -- ALGLIB routine --

                 28.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixmv(int m,
                int n,
                double[,] a,
                int ia,
                int ja,
                int opa,
                double[] x,
                int ix,
                ref double[] y,
                int iy)
            {
                int i = 0;
                double v = 0;
                int i_ = 0;
                int i1_ = 0;

                if (m == 0)
                {
                    return;
                }
                if (n == 0)
                {
                    for (i = 0; i <= m - 1; i++)
                    {
                        y[iy + i] = 0;
                    }
                    return;
                }
                if (ablasf.rmatrixmvf(m, n, a, ia, ja, opa, x, ix, ref y, iy))
                {
                    return;
                }
                if (opa == 0)
                {

                    //
                    // y = A*x
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        i1_ = (ix) - (ja);
                        v = 0.0;
                        for (i_ = ja; i_ <= ja + n - 1; i_++)
                        {
                            v += a[ia + i, i_] * x[i_ + i1_];
                        }
                        y[iy + i] = v;
                    }
                    return;
                }
                if (opa == 1)
                {

                    //
                    // y = A^T*x
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        y[iy + i] = 0;
                    }
                    for (i = 0; i <= n - 1; i++)
                    {
                        v = x[ix + i];
                        i1_ = (ja) - (iy);
                        for (i_ = iy; i_ <= iy + m - 1; i_++)
                        {
                            y[i_] = y[i_] + v * a[ia + i, i_ + i1_];
                        }
                    }
                    return;
                }
            }


            /*************************************************************************
            This subroutine calculates X*op(A^-1) where:
            * X is MxN general matrix
            * A is NxN upper/lower triangular/unitriangular matrix
            * "op" may be identity transformation, transposition, conjugate transposition

            Multiplication result replaces X.
            Cache-oblivious algorithm is used.

            INPUT PARAMETERS
                N   -   matrix size, N>=0
                M   -   matrix size, N>=0
                A       -   matrix, actial matrix is stored in A[I1:I1+N-1,J1:J1+N-1]
                I1      -   submatrix offset
                J1      -   submatrix offset
                IsUpper -   whether matrix is upper triangular
                IsUnit  -   whether matrix is unitriangular
                OpType  -   transformation type:
                            * 0 - no transformation
                            * 1 - transposition
                            * 2 - conjugate transposition
                C   -   matrix, actial matrix is stored in C[I2:I2+M-1,J2:J2+N-1]
                I2  -   submatrix offset
                J2  -   submatrix offset

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void cmatrixrighttrsm(int m,
                int n,
                complex[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref complex[,] x,
                int i2,
                int j2)
            {
                int s1 = 0;
                int s2 = 0;
                int bs = 0;

                bs = ablascomplexblocksize(a);
                if (m <= bs & n <= bs)
                {
                    cmatrixrighttrsm2(m, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                    return;
                }
                if (m >= n)
                {

                    //
                    // Split X: X*A = (X1 X2)^T*A
                    //
                    ablascomplexsplitlength(a, m, ref s1, ref s2);
                    cmatrixrighttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                    cmatrixrighttrsm(s2, n, a, i1, j1, isupper, isunit, optype, ref x, i2 + s1, j2);
                }
                else
                {

                    //
                    // Split A:
                    //               (A1  A12)
                    // X*op(A) = X*op(       )
                    //               (     A2)
                    //
                    // Different variants depending on
                    // IsUpper/OpType combinations
                    //
                    ablascomplexsplitlength(a, n, ref s1, ref s2);
                    if (isupper & optype == 0)
                    {

                        //
                        //                  (A1  A12)-1
                        // X*A^-1 = (X1 X2)*(       )
                        //                  (     A2)
                        //
                        cmatrixrighttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        cmatrixgemm(m, s2, s1, -1.0, x, i2, j2, 0, a, i1, j1 + s1, 0, 1.0, ref x, i2, j2 + s1);
                        cmatrixrighttrsm(m, s2, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2, j2 + s1);
                        return;
                    }
                    if (isupper & optype != 0)
                    {

                        //
                        //                  (A1'     )-1
                        // X*A^-1 = (X1 X2)*(        )
                        //                  (A12' A2')
                        //
                        cmatrixrighttrsm(m, s2, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2, j2 + s1);
                        cmatrixgemm(m, s1, s2, -1.0, x, i2, j2 + s1, 0, a, i1, j1 + s1, optype, 1.0, ref x, i2, j2);
                        cmatrixrighttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        return;
                    }
                    if (!isupper & optype == 0)
                    {

                        //
                        //                  (A1     )-1
                        // X*A^-1 = (X1 X2)*(       )
                        //                  (A21  A2)
                        //
                        cmatrixrighttrsm(m, s2, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2, j2 + s1);
                        cmatrixgemm(m, s1, s2, -1.0, x, i2, j2 + s1, 0, a, i1 + s1, j1, 0, 1.0, ref x, i2, j2);
                        cmatrixrighttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        return;
                    }
                    if (!isupper & optype != 0)
                    {

                        //
                        //                  (A1' A21')-1
                        // X*A^-1 = (X1 X2)*(        )
                        //                  (     A2')
                        //
                        cmatrixrighttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        cmatrixgemm(m, s2, s1, -1.0, x, i2, j2, 0, a, i1 + s1, j1, optype, 1.0, ref x, i2, j2 + s1);
                        cmatrixrighttrsm(m, s2, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2, j2 + s1);
                        return;
                    }
                }
            }


            /*************************************************************************
            This subroutine calculates op(A^-1)*X where:
            * X is MxN general matrix
            * A is MxM upper/lower triangular/unitriangular matrix
            * "op" may be identity transformation, transposition, conjugate transposition

            Multiplication result replaces X.
            Cache-oblivious algorithm is used.

            INPUT PARAMETERS
                N   -   matrix size, N>=0
                M   -   matrix size, N>=0
                A       -   matrix, actial matrix is stored in A[I1:I1+M-1,J1:J1+M-1]
                I1      -   submatrix offset
                J1      -   submatrix offset
                IsUpper -   whether matrix is upper triangular
                IsUnit  -   whether matrix is unitriangular
                OpType  -   transformation type:
                            * 0 - no transformation
                            * 1 - transposition
                            * 2 - conjugate transposition
                C   -   matrix, actial matrix is stored in C[I2:I2+M-1,J2:J2+N-1]
                I2  -   submatrix offset
                J2  -   submatrix offset

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void cmatrixlefttrsm(int m,
                int n,
                complex[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref complex[,] x,
                int i2,
                int j2)
            {
                int s1 = 0;
                int s2 = 0;
                int bs = 0;

                bs = ablascomplexblocksize(a);
                if (m <= bs & n <= bs)
                {
                    cmatrixlefttrsm2(m, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                    return;
                }
                if (n >= m)
                {

                    //
                    // Split X: op(A)^-1*X = op(A)^-1*(X1 X2)
                    //
                    ablascomplexsplitlength(x, n, ref s1, ref s2);
                    cmatrixlefttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                    cmatrixlefttrsm(m, s2, a, i1, j1, isupper, isunit, optype, ref x, i2, j2 + s1);
                }
                else
                {

                    //
                    // Split A
                    //
                    ablascomplexsplitlength(a, m, ref s1, ref s2);
                    if (isupper & optype == 0)
                    {

                        //
                        //           (A1  A12)-1  ( X1 )
                        // A^-1*X* = (       )   *(    )
                        //           (     A2)    ( X2 )
                        //
                        cmatrixlefttrsm(s2, n, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2 + s1, j2);
                        cmatrixgemm(s1, n, s2, -1.0, a, i1, j1 + s1, 0, x, i2 + s1, j2, 0, 1.0, ref x, i2, j2);
                        cmatrixlefttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        return;
                    }
                    if (isupper & optype != 0)
                    {

                        //
                        //          (A1'     )-1 ( X1 )
                        // A^-1*X = (        )  *(    )
                        //          (A12' A2')   ( X2 )
                        //
                        cmatrixlefttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        cmatrixgemm(s2, n, s1, -1.0, a, i1, j1 + s1, optype, x, i2, j2, 0, 1.0, ref x, i2 + s1, j2);
                        cmatrixlefttrsm(s2, n, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2 + s1, j2);
                        return;
                    }
                    if (!isupper & optype == 0)
                    {

                        //
                        //          (A1     )-1 ( X1 )
                        // A^-1*X = (       )  *(    )
                        //          (A21  A2)   ( X2 )
                        //
                        cmatrixlefttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        cmatrixgemm(s2, n, s1, -1.0, a, i1 + s1, j1, 0, x, i2, j2, 0, 1.0, ref x, i2 + s1, j2);
                        cmatrixlefttrsm(s2, n, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2 + s1, j2);
                        return;
                    }
                    if (!isupper & optype != 0)
                    {

                        //
                        //          (A1' A21')-1 ( X1 )
                        // A^-1*X = (        )  *(    )
                        //          (     A2')   ( X2 )
                        //
                        cmatrixlefttrsm(s2, n, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2 + s1, j2);
                        cmatrixgemm(s1, n, s2, -1.0, a, i1 + s1, j1, optype, x, i2 + s1, j2, 0, 1.0, ref x, i2, j2);
                        cmatrixlefttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        return;
                    }
                }
            }


            /*************************************************************************
            Same as CMatrixRightTRSM, but for real matrices

            OpType may be only 0 or 1.

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixrighttrsm(int m,
                int n,
                double[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref double[,] x,
                int i2,
                int j2)
            {
                int s1 = 0;
                int s2 = 0;
                int bs = 0;

                bs = ablasblocksize(a);
                if (m <= bs & n <= bs)
                {
                    rmatrixrighttrsm2(m, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                    return;
                }
                if (m >= n)
                {

                    //
                    // Split X: X*A = (X1 X2)^T*A
                    //
                    ablassplitlength(a, m, ref s1, ref s2);
                    rmatrixrighttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                    rmatrixrighttrsm(s2, n, a, i1, j1, isupper, isunit, optype, ref x, i2 + s1, j2);
                }
                else
                {

                    //
                    // Split A:
                    //               (A1  A12)
                    // X*op(A) = X*op(       )
                    //               (     A2)
                    //
                    // Different variants depending on
                    // IsUpper/OpType combinations
                    //
                    ablassplitlength(a, n, ref s1, ref s2);
                    if (isupper & optype == 0)
                    {

                        //
                        //                  (A1  A12)-1
                        // X*A^-1 = (X1 X2)*(       )
                        //                  (     A2)
                        //
                        rmatrixrighttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        rmatrixgemm(m, s2, s1, -1.0, x, i2, j2, 0, a, i1, j1 + s1, 0, 1.0, ref x, i2, j2 + s1);
                        rmatrixrighttrsm(m, s2, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2, j2 + s1);
                        return;
                    }
                    if (isupper & optype != 0)
                    {

                        //
                        //                  (A1'     )-1
                        // X*A^-1 = (X1 X2)*(        )
                        //                  (A12' A2')
                        //
                        rmatrixrighttrsm(m, s2, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2, j2 + s1);
                        rmatrixgemm(m, s1, s2, -1.0, x, i2, j2 + s1, 0, a, i1, j1 + s1, optype, 1.0, ref x, i2, j2);
                        rmatrixrighttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        return;
                    }
                    if (!isupper & optype == 0)
                    {

                        //
                        //                  (A1     )-1
                        // X*A^-1 = (X1 X2)*(       )
                        //                  (A21  A2)
                        //
                        rmatrixrighttrsm(m, s2, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2, j2 + s1);
                        rmatrixgemm(m, s1, s2, -1.0, x, i2, j2 + s1, 0, a, i1 + s1, j1, 0, 1.0, ref x, i2, j2);
                        rmatrixrighttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        return;
                    }
                    if (!isupper & optype != 0)
                    {

                        //
                        //                  (A1' A21')-1
                        // X*A^-1 = (X1 X2)*(        )
                        //                  (     A2')
                        //
                        rmatrixrighttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        rmatrixgemm(m, s2, s1, -1.0, x, i2, j2, 0, a, i1 + s1, j1, optype, 1.0, ref x, i2, j2 + s1);
                        rmatrixrighttrsm(m, s2, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2, j2 + s1);
                        return;
                    }
                }
            }


            /*************************************************************************
            Same as CMatrixLeftTRSM, but for real matrices

            OpType may be only 0 or 1.

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixlefttrsm(int m,
                int n,
                double[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref double[,] x,
                int i2,
                int j2)
            {
                int s1 = 0;
                int s2 = 0;
                int bs = 0;

                bs = ablasblocksize(a);
                if (m <= bs & n <= bs)
                {
                    rmatrixlefttrsm2(m, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                    return;
                }
                if (n >= m)
                {

                    //
                    // Split X: op(A)^-1*X = op(A)^-1*(X1 X2)
                    //
                    ablassplitlength(x, n, ref s1, ref s2);
                    rmatrixlefttrsm(m, s1, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                    rmatrixlefttrsm(m, s2, a, i1, j1, isupper, isunit, optype, ref x, i2, j2 + s1);
                }
                else
                {

                    //
                    // Split A
                    //
                    ablassplitlength(a, m, ref s1, ref s2);
                    if (isupper & optype == 0)
                    {

                        //
                        //           (A1  A12)-1  ( X1 )
                        // A^-1*X* = (       )   *(    )
                        //           (     A2)    ( X2 )
                        //
                        rmatrixlefttrsm(s2, n, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2 + s1, j2);
                        rmatrixgemm(s1, n, s2, -1.0, a, i1, j1 + s1, 0, x, i2 + s1, j2, 0, 1.0, ref x, i2, j2);
                        rmatrixlefttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        return;
                    }
                    if (isupper & optype != 0)
                    {

                        //
                        //          (A1'     )-1 ( X1 )
                        // A^-1*X = (        )  *(    )
                        //          (A12' A2')   ( X2 )
                        //
                        rmatrixlefttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        rmatrixgemm(s2, n, s1, -1.0, a, i1, j1 + s1, optype, x, i2, j2, 0, 1.0, ref x, i2 + s1, j2);
                        rmatrixlefttrsm(s2, n, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2 + s1, j2);
                        return;
                    }
                    if (!isupper & optype == 0)
                    {

                        //
                        //          (A1     )-1 ( X1 )
                        // A^-1*X = (       )  *(    )
                        //          (A21  A2)   ( X2 )
                        //
                        rmatrixlefttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        rmatrixgemm(s2, n, s1, -1.0, a, i1 + s1, j1, 0, x, i2, j2, 0, 1.0, ref x, i2 + s1, j2);
                        rmatrixlefttrsm(s2, n, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2 + s1, j2);
                        return;
                    }
                    if (!isupper & optype != 0)
                    {

                        //
                        //          (A1' A21')-1 ( X1 )
                        // A^-1*X = (        )  *(    )
                        //          (     A2')   ( X2 )
                        //
                        rmatrixlefttrsm(s2, n, a, i1 + s1, j1 + s1, isupper, isunit, optype, ref x, i2 + s1, j2);
                        rmatrixgemm(s1, n, s2, -1.0, a, i1 + s1, j1, optype, x, i2 + s1, j2, 0, 1.0, ref x, i2, j2);
                        rmatrixlefttrsm(s1, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2);
                        return;
                    }
                }
            }


            /*************************************************************************
            This subroutine calculates  C=alpha*A*A^H+beta*C  or  C=alpha*A^H*A+beta*C
            where:
            * C is NxN Hermitian matrix given by its upper/lower triangle
            * A is NxK matrix when A*A^H is calculated, KxN matrix otherwise

            Additional info:
            * cache-oblivious algorithm is used.
            * multiplication result replaces C. If Beta=0, C elements are not used in
              calculations (not multiplied by zero - just not referenced)
            * if Alpha=0, A is not used (not multiplied by zero - just not referenced)
            * if both Beta and Alpha are zero, C is filled by zeros.

            INPUT PARAMETERS
                N       -   matrix size, N>=0
                K       -   matrix size, K>=0
                Alpha   -   coefficient
                A       -   matrix
                IA      -   submatrix offset
                JA      -   submatrix offset
                OpTypeA -   multiplication type:
                            * 0 - A*A^H is calculated
                            * 2 - A^H*A is calculated
                Beta    -   coefficient
                C       -   matrix
                IC      -   submatrix offset
                JC      -   submatrix offset
                IsUpper -   whether C is upper triangular or lower triangular

              -- ALGLIB routine --
                 16.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void cmatrixsyrk(int n,
                int k,
                double alpha,
                complex[,] a,
                int ia,
                int ja,
                int optypea,
                double beta,
                ref complex[,] c,
                int ic,
                int jc,
                bool isupper)
            {
                int s1 = 0;
                int s2 = 0;
                int bs = 0;

                bs = ablascomplexblocksize(a);
                if (n <= bs & k <= bs)
                {
                    cmatrixsyrk2(n, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                    return;
                }
                if (k >= n)
                {

                    //
                    // Split K
                    //
                    ablascomplexsplitlength(a, k, ref s1, ref s2);
                    if (optypea == 0)
                    {
                        cmatrixsyrk(n, s1, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        cmatrixsyrk(n, s2, alpha, a, ia, ja + s1, optypea, 1.0, ref c, ic, jc, isupper);
                    }
                    else
                    {
                        cmatrixsyrk(n, s1, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        cmatrixsyrk(n, s2, alpha, a, ia + s1, ja, optypea, 1.0, ref c, ic, jc, isupper);
                    }
                }
                else
                {

                    //
                    // Split N
                    //
                    ablascomplexsplitlength(a, n, ref s1, ref s2);
                    if (optypea == 0 & isupper)
                    {
                        cmatrixsyrk(s1, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        cmatrixgemm(s1, s2, k, alpha, a, ia, ja, 0, a, ia + s1, ja, 2, beta, ref c, ic, jc + s1);
                        cmatrixsyrk(s2, k, alpha, a, ia + s1, ja, optypea, beta, ref c, ic + s1, jc + s1, isupper);
                        return;
                    }
                    if (optypea == 0 & !isupper)
                    {
                        cmatrixsyrk(s1, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        cmatrixgemm(s2, s1, k, alpha, a, ia + s1, ja, 0, a, ia, ja, 2, beta, ref c, ic + s1, jc);
                        cmatrixsyrk(s2, k, alpha, a, ia + s1, ja, optypea, beta, ref c, ic + s1, jc + s1, isupper);
                        return;
                    }
                    if (optypea != 0 & isupper)
                    {
                        cmatrixsyrk(s1, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        cmatrixgemm(s1, s2, k, alpha, a, ia, ja, 2, a, ia, ja + s1, 0, beta, ref c, ic, jc + s1);
                        cmatrixsyrk(s2, k, alpha, a, ia, ja + s1, optypea, beta, ref c, ic + s1, jc + s1, isupper);
                        return;
                    }
                    if (optypea != 0 & !isupper)
                    {
                        cmatrixsyrk(s1, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        cmatrixgemm(s2, s1, k, alpha, a, ia, ja + s1, 2, a, ia, ja, 0, beta, ref c, ic + s1, jc);
                        cmatrixsyrk(s2, k, alpha, a, ia, ja + s1, optypea, beta, ref c, ic + s1, jc + s1, isupper);
                        return;
                    }
                }
            }


            /*************************************************************************
            Same as CMatrixSYRK, but for real matrices

            OpType may be only 0 or 1.

              -- ALGLIB routine --
                 16.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixsyrk(int n,
                int k,
                double alpha,
                double[,] a,
                int ia,
                int ja,
                int optypea,
                double beta,
                ref double[,] c,
                int ic,
                int jc,
                bool isupper)
            {
                int s1 = 0;
                int s2 = 0;
                int bs = 0;

                bs = ablasblocksize(a);
                if (n <= bs & k <= bs)
                {
                    rmatrixsyrk2(n, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                    return;
                }
                if (k >= n)
                {

                    //
                    // Split K
                    //
                    ablassplitlength(a, k, ref s1, ref s2);
                    if (optypea == 0)
                    {
                        rmatrixsyrk(n, s1, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        rmatrixsyrk(n, s2, alpha, a, ia, ja + s1, optypea, 1.0, ref c, ic, jc, isupper);
                    }
                    else
                    {
                        rmatrixsyrk(n, s1, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        rmatrixsyrk(n, s2, alpha, a, ia + s1, ja, optypea, 1.0, ref c, ic, jc, isupper);
                    }
                }
                else
                {

                    //
                    // Split N
                    //
                    ablassplitlength(a, n, ref s1, ref s2);
                    if (optypea == 0 & isupper)
                    {
                        rmatrixsyrk(s1, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        rmatrixgemm(s1, s2, k, alpha, a, ia, ja, 0, a, ia + s1, ja, 1, beta, ref c, ic, jc + s1);
                        rmatrixsyrk(s2, k, alpha, a, ia + s1, ja, optypea, beta, ref c, ic + s1, jc + s1, isupper);
                        return;
                    }
                    if (optypea == 0 & !isupper)
                    {
                        rmatrixsyrk(s1, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        rmatrixgemm(s2, s1, k, alpha, a, ia + s1, ja, 0, a, ia, ja, 1, beta, ref c, ic + s1, jc);
                        rmatrixsyrk(s2, k, alpha, a, ia + s1, ja, optypea, beta, ref c, ic + s1, jc + s1, isupper);
                        return;
                    }
                    if (optypea != 0 & isupper)
                    {
                        rmatrixsyrk(s1, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        rmatrixgemm(s1, s2, k, alpha, a, ia, ja, 1, a, ia, ja + s1, 0, beta, ref c, ic, jc + s1);
                        rmatrixsyrk(s2, k, alpha, a, ia, ja + s1, optypea, beta, ref c, ic + s1, jc + s1, isupper);
                        return;
                    }
                    if (optypea != 0 & !isupper)
                    {
                        rmatrixsyrk(s1, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper);
                        rmatrixgemm(s2, s1, k, alpha, a, ia, ja + s1, 1, a, ia, ja, 0, beta, ref c, ic + s1, jc);
                        rmatrixsyrk(s2, k, alpha, a, ia, ja + s1, optypea, beta, ref c, ic + s1, jc + s1, isupper);
                        return;
                    }
                }
            }


            /*************************************************************************
            This subroutine calculates C = alpha*op1(A)*op2(B) +beta*C where:
            * C is MxN general matrix
            * op1(A) is MxK matrix
            * op2(B) is KxN matrix
            * "op" may be identity transformation, transposition, conjugate transposition

            Additional info:
            * cache-oblivious algorithm is used.
            * multiplication result replaces C. If Beta=0, C elements are not used in
              calculations (not multiplied by zero - just not referenced)
            * if Alpha=0, A is not used (not multiplied by zero - just not referenced)
            * if both Beta and Alpha are zero, C is filled by zeros.

            INPUT PARAMETERS
                N       -   matrix size, N>0
                M       -   matrix size, N>0
                K       -   matrix size, K>0
                Alpha   -   coefficient
                A       -   matrix
                IA      -   submatrix offset
                JA      -   submatrix offset
                OpTypeA -   transformation type:
                            * 0 - no transformation
                            * 1 - transposition
                            * 2 - conjugate transposition
                B       -   matrix
                IB      -   submatrix offset
                JB      -   submatrix offset
                OpTypeB -   transformation type:
                            * 0 - no transformation
                            * 1 - transposition
                            * 2 - conjugate transposition
                Beta    -   coefficient
                C       -   matrix
                IC      -   submatrix offset
                JC      -   submatrix offset

              -- ALGLIB routine --
                 16.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void cmatrixgemm(int m,
                int n,
                int k,
                complex alpha,
                complex[,] a,
                int ia,
                int ja,
                int optypea,
                complex[,] b,
                int ib,
                int jb,
                int optypeb,
                complex beta,
                ref complex[,] c,
                int ic,
                int jc)
            {
                int s1 = 0;
                int s2 = 0;
                int bs = 0;

                bs = ablascomplexblocksize(a);
                if ((m <= bs & n <= bs) & k <= bs)
                {
                    cmatrixgemmk(m, n, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                    return;
                }
                if (m >= n & m >= k)
                {

                    //
                    // A*B = (A1 A2)^T*B
                    //
                    ablascomplexsplitlength(a, m, ref s1, ref s2);
                    cmatrixgemm(s1, n, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                    if (optypea == 0)
                    {
                        cmatrixgemm(s2, n, k, alpha, a, ia + s1, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic + s1, jc);
                    }
                    else
                    {
                        cmatrixgemm(s2, n, k, alpha, a, ia, ja + s1, optypea, b, ib, jb, optypeb, beta, ref c, ic + s1, jc);
                    }
                    return;
                }
                if (n >= m & n >= k)
                {

                    //
                    // A*B = A*(B1 B2)
                    //
                    ablascomplexsplitlength(a, n, ref s1, ref s2);
                    if (optypeb == 0)
                    {
                        cmatrixgemm(m, s1, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        cmatrixgemm(m, s2, k, alpha, a, ia, ja, optypea, b, ib, jb + s1, optypeb, beta, ref c, ic, jc + s1);
                    }
                    else
                    {
                        cmatrixgemm(m, s1, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        cmatrixgemm(m, s2, k, alpha, a, ia, ja, optypea, b, ib + s1, jb, optypeb, beta, ref c, ic, jc + s1);
                    }
                    return;
                }
                if (k >= m & k >= n)
                {

                    //
                    // A*B = (A1 A2)*(B1 B2)^T
                    //
                    ablascomplexsplitlength(a, k, ref s1, ref s2);
                    if (optypea == 0 & optypeb == 0)
                    {
                        cmatrixgemm(m, n, s1, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        cmatrixgemm(m, n, s2, alpha, a, ia, ja + s1, optypea, b, ib + s1, jb, optypeb, 1.0, ref c, ic, jc);
                    }
                    if (optypea == 0 & optypeb != 0)
                    {
                        cmatrixgemm(m, n, s1, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        cmatrixgemm(m, n, s2, alpha, a, ia, ja + s1, optypea, b, ib, jb + s1, optypeb, 1.0, ref c, ic, jc);
                    }
                    if (optypea != 0 & optypeb == 0)
                    {
                        cmatrixgemm(m, n, s1, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        cmatrixgemm(m, n, s2, alpha, a, ia + s1, ja, optypea, b, ib + s1, jb, optypeb, 1.0, ref c, ic, jc);
                    }
                    if (optypea != 0 & optypeb != 0)
                    {
                        cmatrixgemm(m, n, s1, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        cmatrixgemm(m, n, s2, alpha, a, ia + s1, ja, optypea, b, ib, jb + s1, optypeb, 1.0, ref c, ic, jc);
                    }
                    return;
                }
            }


            /*************************************************************************
            Same as CMatrixGEMM, but for real numbers.
            OpType may be only 0 or 1.

              -- ALGLIB routine --
                 16.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixgemm(int m,
                int n,
                int k,
                double alpha,
                double[,] a,
                int ia,
                int ja,
                int optypea,
                double[,] b,
                int ib,
                int jb,
                int optypeb,
                double beta,
                ref double[,] c,
                int ic,
                int jc)
            {
                int s1 = 0;
                int s2 = 0;
                int bs = 0;

                bs = ablasblocksize(a);
                if ((m <= bs & n <= bs) & k <= bs)
                {
                    rmatrixgemmk(m, n, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                    return;
                }
                if (m >= n & m >= k)
                {

                    //
                    // A*B = (A1 A2)^T*B
                    //
                    ablassplitlength(a, m, ref s1, ref s2);
                    if (optypea == 0)
                    {
                        rmatrixgemm(s1, n, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        rmatrixgemm(s2, n, k, alpha, a, ia + s1, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic + s1, jc);
                    }
                    else
                    {
                        rmatrixgemm(s1, n, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        rmatrixgemm(s2, n, k, alpha, a, ia, ja + s1, optypea, b, ib, jb, optypeb, beta, ref c, ic + s1, jc);
                    }
                    return;
                }
                if (n >= m & n >= k)
                {

                    //
                    // A*B = A*(B1 B2)
                    //
                    ablassplitlength(a, n, ref s1, ref s2);
                    if (optypeb == 0)
                    {
                        rmatrixgemm(m, s1, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        rmatrixgemm(m, s2, k, alpha, a, ia, ja, optypea, b, ib, jb + s1, optypeb, beta, ref c, ic, jc + s1);
                    }
                    else
                    {
                        rmatrixgemm(m, s1, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        rmatrixgemm(m, s2, k, alpha, a, ia, ja, optypea, b, ib + s1, jb, optypeb, beta, ref c, ic, jc + s1);
                    }
                    return;
                }
                if (k >= m & k >= n)
                {

                    //
                    // A*B = (A1 A2)*(B1 B2)^T
                    //
                    ablassplitlength(a, k, ref s1, ref s2);
                    if (optypea == 0 & optypeb == 0)
                    {
                        rmatrixgemm(m, n, s1, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        rmatrixgemm(m, n, s2, alpha, a, ia, ja + s1, optypea, b, ib + s1, jb, optypeb, 1.0, ref c, ic, jc);
                    }
                    if (optypea == 0 & optypeb != 0)
                    {
                        rmatrixgemm(m, n, s1, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        rmatrixgemm(m, n, s2, alpha, a, ia, ja + s1, optypea, b, ib, jb + s1, optypeb, 1.0, ref c, ic, jc);
                    }
                    if (optypea != 0 & optypeb == 0)
                    {
                        rmatrixgemm(m, n, s1, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        rmatrixgemm(m, n, s2, alpha, a, ia + s1, ja, optypea, b, ib + s1, jb, optypeb, 1.0, ref c, ic, jc);
                    }
                    if (optypea != 0 & optypeb != 0)
                    {
                        rmatrixgemm(m, n, s1, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc);
                        rmatrixgemm(m, n, s2, alpha, a, ia + s1, ja, optypea, b, ib, jb + s1, optypeb, 1.0, ref c, ic, jc);
                    }
                    return;
                }
            }


            /*************************************************************************
            Complex ABLASSplitLength

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            private static void ablasinternalsplitlength(int n,
                int nb,
                ref int n1,
                ref int n2)
            {
                int r = 0;

                n1 = 0;
                n2 = 0;

                if (n <= nb)
                {

                    //
                    // Block size, no further splitting
                    //
                    n1 = n;
                    n2 = 0;
                }
                else
                {

                    //
                    // Greater than block size
                    //
                    if (n % nb != 0)
                    {

                        //
                        // Split remainder
                        //
                        n2 = n % nb;
                        n1 = n - n2;
                    }
                    else
                    {

                        //
                        // Split on block boundaries
                        //
                        n2 = n / 2;
                        n1 = n - n2;
                        if (n1 % nb == 0)
                        {
                            return;
                        }
                        r = nb - n1 % nb;
                        n1 = n1 + r;
                        n2 = n2 - r;
                    }
                }
            }


            /*************************************************************************
            Level 2 variant of CMatrixRightTRSM
            *************************************************************************/
            private static void cmatrixrighttrsm2(int m,
                int n,
                complex[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref complex[,] x,
                int i2,
                int j2)
            {
                int i = 0;
                int j = 0;
                complex vc = 0;
                complex vd = 0;
                int i_ = 0;
                int i1_ = 0;


                //
                // Special case
                //
                if (n * m == 0)
                {
                    return;
                }

                //
                // Try to call fast TRSM
                //
                if (ablasf.cmatrixrighttrsmf(m, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2))
                {
                    return;
                }

                //
                // General case
                //
                if (isupper)
                {

                    //
                    // Upper triangular matrix
                    //
                    if (optype == 0)
                    {

                        //
                        // X*A^(-1)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                if (isunit)
                                {
                                    vd = 1;
                                }
                                else
                                {
                                    vd = a[i1 + j, j1 + j];
                                }
                                x[i2 + i, j2 + j] = x[i2 + i, j2 + j] / vd;
                                if (j < n - 1)
                                {
                                    vc = x[i2 + i, j2 + j];
                                    i1_ = (j1 + j + 1) - (j2 + j + 1);
                                    for (i_ = j2 + j + 1; i_ <= j2 + n - 1; i_++)
                                    {
                                        x[i2 + i, i_] = x[i2 + i, i_] - vc * a[i1 + j, i_ + i1_];
                                    }
                                }
                            }
                        }
                        return;
                    }
                    if (optype == 1)
                    {

                        //
                        // X*A^(-T)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = n - 1; j >= 0; j--)
                            {
                                vc = 0;
                                vd = 1;
                                if (j < n - 1)
                                {
                                    i1_ = (j1 + j + 1) - (j2 + j + 1);
                                    vc = 0.0;
                                    for (i_ = j2 + j + 1; i_ <= j2 + n - 1; i_++)
                                    {
                                        vc += x[i2 + i, i_] * a[i1 + j, i_ + i1_];
                                    }
                                }
                                if (!isunit)
                                {
                                    vd = a[i1 + j, j1 + j];
                                }
                                x[i2 + i, j2 + j] = (x[i2 + i, j2 + j] - vc) / vd;
                            }
                        }
                        return;
                    }
                    if (optype == 2)
                    {

                        //
                        // X*A^(-H)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = n - 1; j >= 0; j--)
                            {
                                vc = 0;
                                vd = 1;
                                if (j < n - 1)
                                {
                                    i1_ = (j1 + j + 1) - (j2 + j + 1);
                                    vc = 0.0;
                                    for (i_ = j2 + j + 1; i_ <= j2 + n - 1; i_++)
                                    {
                                        vc += x[i2 + i, i_] * math.conj(a[i1 + j, i_ + i1_]);
                                    }
                                }
                                if (!isunit)
                                {
                                    vd = math.conj(a[i1 + j, j1 + j]);
                                }
                                x[i2 + i, j2 + j] = (x[i2 + i, j2 + j] - vc) / vd;
                            }
                        }
                        return;
                    }
                }
                else
                {

                    //
                    // Lower triangular matrix
                    //
                    if (optype == 0)
                    {

                        //
                        // X*A^(-1)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = n - 1; j >= 0; j--)
                            {
                                if (isunit)
                                {
                                    vd = 1;
                                }
                                else
                                {
                                    vd = a[i1 + j, j1 + j];
                                }
                                x[i2 + i, j2 + j] = x[i2 + i, j2 + j] / vd;
                                if (j > 0)
                                {
                                    vc = x[i2 + i, j2 + j];
                                    i1_ = (j1) - (j2);
                                    for (i_ = j2; i_ <= j2 + j - 1; i_++)
                                    {
                                        x[i2 + i, i_] = x[i2 + i, i_] - vc * a[i1 + j, i_ + i1_];
                                    }
                                }
                            }
                        }
                        return;
                    }
                    if (optype == 1)
                    {

                        //
                        // X*A^(-T)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                vc = 0;
                                vd = 1;
                                if (j > 0)
                                {
                                    i1_ = (j1) - (j2);
                                    vc = 0.0;
                                    for (i_ = j2; i_ <= j2 + j - 1; i_++)
                                    {
                                        vc += x[i2 + i, i_] * a[i1 + j, i_ + i1_];
                                    }
                                }
                                if (!isunit)
                                {
                                    vd = a[i1 + j, j1 + j];
                                }
                                x[i2 + i, j2 + j] = (x[i2 + i, j2 + j] - vc) / vd;
                            }
                        }
                        return;
                    }
                    if (optype == 2)
                    {

                        //
                        // X*A^(-H)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                vc = 0;
                                vd = 1;
                                if (j > 0)
                                {
                                    i1_ = (j1) - (j2);
                                    vc = 0.0;
                                    for (i_ = j2; i_ <= j2 + j - 1; i_++)
                                    {
                                        vc += x[i2 + i, i_] * math.conj(a[i1 + j, i_ + i1_]);
                                    }
                                }
                                if (!isunit)
                                {
                                    vd = math.conj(a[i1 + j, j1 + j]);
                                }
                                x[i2 + i, j2 + j] = (x[i2 + i, j2 + j] - vc) / vd;
                            }
                        }
                        return;
                    }
                }
            }


            /*************************************************************************
            Level-2 subroutine
            *************************************************************************/
            private static void cmatrixlefttrsm2(int m,
                int n,
                complex[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref complex[,] x,
                int i2,
                int j2)
            {
                int i = 0;
                int j = 0;
                complex vc = 0;
                complex vd = 0;
                int i_ = 0;


                //
                // Special case
                //
                if (n * m == 0)
                {
                    return;
                }

                //
                // Try to call fast TRSM
                //
                if (ablasf.cmatrixlefttrsmf(m, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2))
                {
                    return;
                }

                //
                // General case
                //
                if (isupper)
                {

                    //
                    // Upper triangular matrix
                    //
                    if (optype == 0)
                    {

                        //
                        // A^(-1)*X
                        //
                        for (i = m - 1; i >= 0; i--)
                        {
                            for (j = i + 1; j <= m - 1; j++)
                            {
                                vc = a[i1 + i, j1 + j];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + i, i_] = x[i2 + i, i_] - vc * x[i2 + j, i_];
                                }
                            }
                            if (!isunit)
                            {
                                vd = 1 / a[i1 + i, j1 + i];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + i, i_] = vd * x[i2 + i, i_];
                                }
                            }
                        }
                        return;
                    }
                    if (optype == 1)
                    {

                        //
                        // A^(-T)*X
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            if (isunit)
                            {
                                vd = 1;
                            }
                            else
                            {
                                vd = 1 / a[i1 + i, j1 + i];
                            }
                            for (i_ = j2; i_ <= j2 + n - 1; i_++)
                            {
                                x[i2 + i, i_] = vd * x[i2 + i, i_];
                            }
                            for (j = i + 1; j <= m - 1; j++)
                            {
                                vc = a[i1 + i, j1 + j];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + j, i_] = x[i2 + j, i_] - vc * x[i2 + i, i_];
                                }
                            }
                        }
                        return;
                    }
                    if (optype == 2)
                    {

                        //
                        // A^(-H)*X
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            if (isunit)
                            {
                                vd = 1;
                            }
                            else
                            {
                                vd = 1 / math.conj(a[i1 + i, j1 + i]);
                            }
                            for (i_ = j2; i_ <= j2 + n - 1; i_++)
                            {
                                x[i2 + i, i_] = vd * x[i2 + i, i_];
                            }
                            for (j = i + 1; j <= m - 1; j++)
                            {
                                vc = math.conj(a[i1 + i, j1 + j]);
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + j, i_] = x[i2 + j, i_] - vc * x[i2 + i, i_];
                                }
                            }
                        }
                        return;
                    }
                }
                else
                {

                    //
                    // Lower triangular matrix
                    //
                    if (optype == 0)
                    {

                        //
                        // A^(-1)*X
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= i - 1; j++)
                            {
                                vc = a[i1 + i, j1 + j];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + i, i_] = x[i2 + i, i_] - vc * x[i2 + j, i_];
                                }
                            }
                            if (isunit)
                            {
                                vd = 1;
                            }
                            else
                            {
                                vd = 1 / a[i1 + j, j1 + j];
                            }
                            for (i_ = j2; i_ <= j2 + n - 1; i_++)
                            {
                                x[i2 + i, i_] = vd * x[i2 + i, i_];
                            }
                        }
                        return;
                    }
                    if (optype == 1)
                    {

                        //
                        // A^(-T)*X
                        //
                        for (i = m - 1; i >= 0; i--)
                        {
                            if (isunit)
                            {
                                vd = 1;
                            }
                            else
                            {
                                vd = 1 / a[i1 + i, j1 + i];
                            }
                            for (i_ = j2; i_ <= j2 + n - 1; i_++)
                            {
                                x[i2 + i, i_] = vd * x[i2 + i, i_];
                            }
                            for (j = i - 1; j >= 0; j--)
                            {
                                vc = a[i1 + i, j1 + j];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + j, i_] = x[i2 + j, i_] - vc * x[i2 + i, i_];
                                }
                            }
                        }
                        return;
                    }
                    if (optype == 2)
                    {

                        //
                        // A^(-H)*X
                        //
                        for (i = m - 1; i >= 0; i--)
                        {
                            if (isunit)
                            {
                                vd = 1;
                            }
                            else
                            {
                                vd = 1 / math.conj(a[i1 + i, j1 + i]);
                            }
                            for (i_ = j2; i_ <= j2 + n - 1; i_++)
                            {
                                x[i2 + i, i_] = vd * x[i2 + i, i_];
                            }
                            for (j = i - 1; j >= 0; j--)
                            {
                                vc = math.conj(a[i1 + i, j1 + j]);
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + j, i_] = x[i2 + j, i_] - vc * x[i2 + i, i_];
                                }
                            }
                        }
                        return;
                    }
                }
            }


            /*************************************************************************
            Level 2 subroutine

              -- ALGLIB routine --
                 15.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            private static void rmatrixrighttrsm2(int m,
                int n,
                double[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref double[,] x,
                int i2,
                int j2)
            {
                int i = 0;
                int j = 0;
                double vr = 0;
                double vd = 0;
                int i_ = 0;
                int i1_ = 0;


                //
                // Special case
                //
                if (n * m == 0)
                {
                    return;
                }

                //
                // Try to use "fast" code
                //
                if (ablasf.rmatrixrighttrsmf(m, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2))
                {
                    return;
                }

                //
                // General case
                //
                if (isupper)
                {

                    //
                    // Upper triangular matrix
                    //
                    if (optype == 0)
                    {

                        //
                        // X*A^(-1)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                if (isunit)
                                {
                                    vd = 1;
                                }
                                else
                                {
                                    vd = a[i1 + j, j1 + j];
                                }
                                x[i2 + i, j2 + j] = x[i2 + i, j2 + j] / vd;
                                if (j < n - 1)
                                {
                                    vr = x[i2 + i, j2 + j];
                                    i1_ = (j1 + j + 1) - (j2 + j + 1);
                                    for (i_ = j2 + j + 1; i_ <= j2 + n - 1; i_++)
                                    {
                                        x[i2 + i, i_] = x[i2 + i, i_] - vr * a[i1 + j, i_ + i1_];
                                    }
                                }
                            }
                        }
                        return;
                    }
                    if (optype == 1)
                    {

                        //
                        // X*A^(-T)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = n - 1; j >= 0; j--)
                            {
                                vr = 0;
                                vd = 1;
                                if (j < n - 1)
                                {
                                    i1_ = (j1 + j + 1) - (j2 + j + 1);
                                    vr = 0.0;
                                    for (i_ = j2 + j + 1; i_ <= j2 + n - 1; i_++)
                                    {
                                        vr += x[i2 + i, i_] * a[i1 + j, i_ + i1_];
                                    }
                                }
                                if (!isunit)
                                {
                                    vd = a[i1 + j, j1 + j];
                                }
                                x[i2 + i, j2 + j] = (x[i2 + i, j2 + j] - vr) / vd;
                            }
                        }
                        return;
                    }
                }
                else
                {

                    //
                    // Lower triangular matrix
                    //
                    if (optype == 0)
                    {

                        //
                        // X*A^(-1)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = n - 1; j >= 0; j--)
                            {
                                if (isunit)
                                {
                                    vd = 1;
                                }
                                else
                                {
                                    vd = a[i1 + j, j1 + j];
                                }
                                x[i2 + i, j2 + j] = x[i2 + i, j2 + j] / vd;
                                if (j > 0)
                                {
                                    vr = x[i2 + i, j2 + j];
                                    i1_ = (j1) - (j2);
                                    for (i_ = j2; i_ <= j2 + j - 1; i_++)
                                    {
                                        x[i2 + i, i_] = x[i2 + i, i_] - vr * a[i1 + j, i_ + i1_];
                                    }
                                }
                            }
                        }
                        return;
                    }
                    if (optype == 1)
                    {

                        //
                        // X*A^(-T)
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                vr = 0;
                                vd = 1;
                                if (j > 0)
                                {
                                    i1_ = (j1) - (j2);
                                    vr = 0.0;
                                    for (i_ = j2; i_ <= j2 + j - 1; i_++)
                                    {
                                        vr += x[i2 + i, i_] * a[i1 + j, i_ + i1_];
                                    }
                                }
                                if (!isunit)
                                {
                                    vd = a[i1 + j, j1 + j];
                                }
                                x[i2 + i, j2 + j] = (x[i2 + i, j2 + j] - vr) / vd;
                            }
                        }
                        return;
                    }
                }
            }


            /*************************************************************************
            Level 2 subroutine
            *************************************************************************/
            private static void rmatrixlefttrsm2(int m,
                int n,
                double[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref double[,] x,
                int i2,
                int j2)
            {
                int i = 0;
                int j = 0;
                double vr = 0;
                double vd = 0;
                int i_ = 0;


                //
                // Special case
                //
                if (n * m == 0)
                {
                    return;
                }

                //
                // Try fast code
                //
                if (ablasf.rmatrixlefttrsmf(m, n, a, i1, j1, isupper, isunit, optype, ref x, i2, j2))
                {
                    return;
                }

                //
                // General case
                //
                if (isupper)
                {

                    //
                    // Upper triangular matrix
                    //
                    if (optype == 0)
                    {

                        //
                        // A^(-1)*X
                        //
                        for (i = m - 1; i >= 0; i--)
                        {
                            for (j = i + 1; j <= m - 1; j++)
                            {
                                vr = a[i1 + i, j1 + j];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + i, i_] = x[i2 + i, i_] - vr * x[i2 + j, i_];
                                }
                            }
                            if (!isunit)
                            {
                                vd = 1 / a[i1 + i, j1 + i];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + i, i_] = vd * x[i2 + i, i_];
                                }
                            }
                        }
                        return;
                    }
                    if (optype == 1)
                    {

                        //
                        // A^(-T)*X
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            if (isunit)
                            {
                                vd = 1;
                            }
                            else
                            {
                                vd = 1 / a[i1 + i, j1 + i];
                            }
                            for (i_ = j2; i_ <= j2 + n - 1; i_++)
                            {
                                x[i2 + i, i_] = vd * x[i2 + i, i_];
                            }
                            for (j = i + 1; j <= m - 1; j++)
                            {
                                vr = a[i1 + i, j1 + j];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + j, i_] = x[i2 + j, i_] - vr * x[i2 + i, i_];
                                }
                            }
                        }
                        return;
                    }
                }
                else
                {

                    //
                    // Lower triangular matrix
                    //
                    if (optype == 0)
                    {

                        //
                        // A^(-1)*X
                        //
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= i - 1; j++)
                            {
                                vr = a[i1 + i, j1 + j];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + i, i_] = x[i2 + i, i_] - vr * x[i2 + j, i_];
                                }
                            }
                            if (isunit)
                            {
                                vd = 1;
                            }
                            else
                            {
                                vd = 1 / a[i1 + j, j1 + j];
                            }
                            for (i_ = j2; i_ <= j2 + n - 1; i_++)
                            {
                                x[i2 + i, i_] = vd * x[i2 + i, i_];
                            }
                        }
                        return;
                    }
                    if (optype == 1)
                    {

                        //
                        // A^(-T)*X
                        //
                        for (i = m - 1; i >= 0; i--)
                        {
                            if (isunit)
                            {
                                vd = 1;
                            }
                            else
                            {
                                vd = 1 / a[i1 + i, j1 + i];
                            }
                            for (i_ = j2; i_ <= j2 + n - 1; i_++)
                            {
                                x[i2 + i, i_] = vd * x[i2 + i, i_];
                            }
                            for (j = i - 1; j >= 0; j--)
                            {
                                vr = a[i1 + i, j1 + j];
                                for (i_ = j2; i_ <= j2 + n - 1; i_++)
                                {
                                    x[i2 + j, i_] = x[i2 + j, i_] - vr * x[i2 + i, i_];
                                }
                            }
                        }
                        return;
                    }
                }
            }


            /*************************************************************************
            Level 2 subroutine
            *************************************************************************/
            private static void cmatrixsyrk2(int n,
                int k,
                double alpha,
                complex[,] a,
                int ia,
                int ja,
                int optypea,
                double beta,
                ref complex[,] c,
                int ic,
                int jc,
                bool isupper)
            {
                int i = 0;
                int j = 0;
                int j1 = 0;
                int j2 = 0;
                complex v = 0;
                int i_ = 0;
                int i1_ = 0;


                //
                // Fast exit (nothing to be done)
                //
                if (((double)(alpha) == (double)(0) | k == 0) & (double)(beta) == (double)(1))
                {
                    return;
                }

                //
                // Try to call fast SYRK
                //
                if (ablasf.cmatrixsyrkf(n, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper))
                {
                    return;
                }

                //
                // SYRK
                //
                if (optypea == 0)
                {

                    //
                    // C=alpha*A*A^H+beta*C
                    //
                    for (i = 0; i <= n - 1; i++)
                    {
                        if (isupper)
                        {
                            j1 = i;
                            j2 = n - 1;
                        }
                        else
                        {
                            j1 = 0;
                            j2 = i;
                        }
                        for (j = j1; j <= j2; j++)
                        {
                            if ((double)(alpha) != (double)(0) & k > 0)
                            {
                                v = 0.0;
                                for (i_ = ja; i_ <= ja + k - 1; i_++)
                                {
                                    v += a[ia + i, i_] * math.conj(a[ia + j, i_]);
                                }
                            }
                            else
                            {
                                v = 0;
                            }
                            if ((double)(beta) == (double)(0))
                            {
                                c[ic + i, jc + j] = alpha * v;
                            }
                            else
                            {
                                c[ic + i, jc + j] = beta * c[ic + i, jc + j] + alpha * v;
                            }
                        }
                    }
                    return;
                }
                else
                {

                    //
                    // C=alpha*A^H*A+beta*C
                    //
                    for (i = 0; i <= n - 1; i++)
                    {
                        if (isupper)
                        {
                            j1 = i;
                            j2 = n - 1;
                        }
                        else
                        {
                            j1 = 0;
                            j2 = i;
                        }
                        if ((double)(beta) == (double)(0))
                        {
                            for (j = j1; j <= j2; j++)
                            {
                                c[ic + i, jc + j] = 0;
                            }
                        }
                        else
                        {
                            for (i_ = jc + j1; i_ <= jc + j2; i_++)
                            {
                                c[ic + i, i_] = beta * c[ic + i, i_];
                            }
                        }
                    }
                    for (i = 0; i <= k - 1; i++)
                    {
                        for (j = 0; j <= n - 1; j++)
                        {
                            if (isupper)
                            {
                                j1 = j;
                                j2 = n - 1;
                            }
                            else
                            {
                                j1 = 0;
                                j2 = j;
                            }
                            v = alpha * math.conj(a[ia + i, ja + j]);
                            i1_ = (ja + j1) - (jc + j1);
                            for (i_ = jc + j1; i_ <= jc + j2; i_++)
                            {
                                c[ic + j, i_] = c[ic + j, i_] + v * a[ia + i, i_ + i1_];
                            }
                        }
                    }
                    return;
                }
            }


            /*************************************************************************
            Level 2 subrotuine
            *************************************************************************/
            private static void rmatrixsyrk2(int n,
                int k,
                double alpha,
                double[,] a,
                int ia,
                int ja,
                int optypea,
                double beta,
                ref double[,] c,
                int ic,
                int jc,
                bool isupper)
            {
                int i = 0;
                int j = 0;
                int j1 = 0;
                int j2 = 0;
                double v = 0;
                int i_ = 0;
                int i1_ = 0;


                //
                // Fast exit (nothing to be done)
                //
                if (((double)(alpha) == (double)(0) | k == 0) & (double)(beta) == (double)(1))
                {
                    return;
                }

                //
                // Try to call fast SYRK
                //
                if (ablasf.rmatrixsyrkf(n, k, alpha, a, ia, ja, optypea, beta, ref c, ic, jc, isupper))
                {
                    return;
                }

                //
                // SYRK
                //
                if (optypea == 0)
                {

                    //
                    // C=alpha*A*A^H+beta*C
                    //
                    for (i = 0; i <= n - 1; i++)
                    {
                        if (isupper)
                        {
                            j1 = i;
                            j2 = n - 1;
                        }
                        else
                        {
                            j1 = 0;
                            j2 = i;
                        }
                        for (j = j1; j <= j2; j++)
                        {
                            if ((double)(alpha) != (double)(0) & k > 0)
                            {
                                v = 0.0;
                                for (i_ = ja; i_ <= ja + k - 1; i_++)
                                {
                                    v += a[ia + i, i_] * a[ia + j, i_];
                                }
                            }
                            else
                            {
                                v = 0;
                            }
                            if ((double)(beta) == (double)(0))
                            {
                                c[ic + i, jc + j] = alpha * v;
                            }
                            else
                            {
                                c[ic + i, jc + j] = beta * c[ic + i, jc + j] + alpha * v;
                            }
                        }
                    }
                    return;
                }
                else
                {

                    //
                    // C=alpha*A^H*A+beta*C
                    //
                    for (i = 0; i <= n - 1; i++)
                    {
                        if (isupper)
                        {
                            j1 = i;
                            j2 = n - 1;
                        }
                        else
                        {
                            j1 = 0;
                            j2 = i;
                        }
                        if ((double)(beta) == (double)(0))
                        {
                            for (j = j1; j <= j2; j++)
                            {
                                c[ic + i, jc + j] = 0;
                            }
                        }
                        else
                        {
                            for (i_ = jc + j1; i_ <= jc + j2; i_++)
                            {
                                c[ic + i, i_] = beta * c[ic + i, i_];
                            }
                        }
                    }
                    for (i = 0; i <= k - 1; i++)
                    {
                        for (j = 0; j <= n - 1; j++)
                        {
                            if (isupper)
                            {
                                j1 = j;
                                j2 = n - 1;
                            }
                            else
                            {
                                j1 = 0;
                                j2 = j;
                            }
                            v = alpha * a[ia + i, ja + j];
                            i1_ = (ja + j1) - (jc + j1);
                            for (i_ = jc + j1; i_ <= jc + j2; i_++)
                            {
                                c[ic + j, i_] = c[ic + j, i_] + v * a[ia + i, i_ + i1_];
                            }
                        }
                    }
                    return;
                }
            }


            /*************************************************************************
            GEMM kernel

              -- ALGLIB routine --
                 16.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            private static void cmatrixgemmk(int m,
                int n,
                int k,
                complex alpha,
                complex[,] a,
                int ia,
                int ja,
                int optypea,
                complex[,] b,
                int ib,
                int jb,
                int optypeb,
                complex beta,
                ref complex[,] c,
                int ic,
                int jc)
            {
                int i = 0;
                int j = 0;
                complex v = 0;
                int i_ = 0;
                int i1_ = 0;


                //
                // Special case
                //
                if (m * n == 0)
                {
                    return;
                }

                //
                // Try optimized code
                //
                if (ablasf.cmatrixgemmf(m, n, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc))
                {
                    return;
                }

                //
                // Another special case
                //
                if (k == 0)
                {
                    if (beta != 0)
                    {
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                c[ic + i, jc + j] = beta * c[ic + i, jc + j];
                            }
                        }
                    }
                    else
                    {
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                c[ic + i, jc + j] = 0;
                            }
                        }
                    }
                    return;
                }

                //
                // General case
                //
                if (optypea == 0 & optypeb != 0)
                {

                    //
                    // A*B'
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        for (j = 0; j <= n - 1; j++)
                        {
                            if (k == 0 | alpha == 0)
                            {
                                v = 0;
                            }
                            else
                            {
                                if (optypeb == 1)
                                {
                                    i1_ = (jb) - (ja);
                                    v = 0.0;
                                    for (i_ = ja; i_ <= ja + k - 1; i_++)
                                    {
                                        v += a[ia + i, i_] * b[ib + j, i_ + i1_];
                                    }
                                }
                                else
                                {
                                    i1_ = (jb) - (ja);
                                    v = 0.0;
                                    for (i_ = ja; i_ <= ja + k - 1; i_++)
                                    {
                                        v += a[ia + i, i_] * math.conj(b[ib + j, i_ + i1_]);
                                    }
                                }
                            }
                            if (beta == 0)
                            {
                                c[ic + i, jc + j] = alpha * v;
                            }
                            else
                            {
                                c[ic + i, jc + j] = beta * c[ic + i, jc + j] + alpha * v;
                            }
                        }
                    }
                    return;
                }
                if (optypea == 0 & optypeb == 0)
                {

                    //
                    // A*B
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        if (beta != 0)
                        {
                            for (i_ = jc; i_ <= jc + n - 1; i_++)
                            {
                                c[ic + i, i_] = beta * c[ic + i, i_];
                            }
                        }
                        else
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                c[ic + i, jc + j] = 0;
                            }
                        }
                        if (alpha != 0)
                        {
                            for (j = 0; j <= k - 1; j++)
                            {
                                v = alpha * a[ia + i, ja + j];
                                i1_ = (jb) - (jc);
                                for (i_ = jc; i_ <= jc + n - 1; i_++)
                                {
                                    c[ic + i, i_] = c[ic + i, i_] + v * b[ib + j, i_ + i1_];
                                }
                            }
                        }
                    }
                    return;
                }
                if (optypea != 0 & optypeb != 0)
                {

                    //
                    // A'*B'
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        for (j = 0; j <= n - 1; j++)
                        {
                            if (alpha == 0)
                            {
                                v = 0;
                            }
                            else
                            {
                                if (optypea == 1)
                                {
                                    if (optypeb == 1)
                                    {
                                        i1_ = (jb) - (ia);
                                        v = 0.0;
                                        for (i_ = ia; i_ <= ia + k - 1; i_++)
                                        {
                                            v += a[i_, ja + i] * b[ib + j, i_ + i1_];
                                        }
                                    }
                                    else
                                    {
                                        i1_ = (jb) - (ia);
                                        v = 0.0;
                                        for (i_ = ia; i_ <= ia + k - 1; i_++)
                                        {
                                            v += a[i_, ja + i] * math.conj(b[ib + j, i_ + i1_]);
                                        }
                                    }
                                }
                                else
                                {
                                    if (optypeb == 1)
                                    {
                                        i1_ = (jb) - (ia);
                                        v = 0.0;
                                        for (i_ = ia; i_ <= ia + k - 1; i_++)
                                        {
                                            v += math.conj(a[i_, ja + i]) * b[ib + j, i_ + i1_];
                                        }
                                    }
                                    else
                                    {
                                        i1_ = (jb) - (ia);
                                        v = 0.0;
                                        for (i_ = ia; i_ <= ia + k - 1; i_++)
                                        {
                                            v += math.conj(a[i_, ja + i]) * math.conj(b[ib + j, i_ + i1_]);
                                        }
                                    }
                                }
                            }
                            if (beta == 0)
                            {
                                c[ic + i, jc + j] = alpha * v;
                            }
                            else
                            {
                                c[ic + i, jc + j] = beta * c[ic + i, jc + j] + alpha * v;
                            }
                        }
                    }
                    return;
                }
                if (optypea != 0 & optypeb == 0)
                {

                    //
                    // A'*B
                    //
                    if (beta == 0)
                    {
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                c[ic + i, jc + j] = 0;
                            }
                        }
                    }
                    else
                    {
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (i_ = jc; i_ <= jc + n - 1; i_++)
                            {
                                c[ic + i, i_] = beta * c[ic + i, i_];
                            }
                        }
                    }
                    if (alpha != 0)
                    {
                        for (j = 0; j <= k - 1; j++)
                        {
                            for (i = 0; i <= m - 1; i++)
                            {
                                if (optypea == 1)
                                {
                                    v = alpha * a[ia + j, ja + i];
                                }
                                else
                                {
                                    v = alpha * math.conj(a[ia + j, ja + i]);
                                }
                                i1_ = (jb) - (jc);
                                for (i_ = jc; i_ <= jc + n - 1; i_++)
                                {
                                    c[ic + i, i_] = c[ic + i, i_] + v * b[ib + j, i_ + i1_];
                                }
                            }
                        }
                    }
                    return;
                }
            }


            /*************************************************************************
            GEMM kernel

              -- ALGLIB routine --
                 16.12.2009
                 Bochkanov Sergey
            *************************************************************************/
            private static void rmatrixgemmk(int m,
                int n,
                int k,
                double alpha,
                double[,] a,
                int ia,
                int ja,
                int optypea,
                double[,] b,
                int ib,
                int jb,
                int optypeb,
                double beta,
                ref double[,] c,
                int ic,
                int jc)
            {
                int i = 0;
                int j = 0;
                double v = 0;
                int i_ = 0;
                int i1_ = 0;


                //
                // if matrix size is zero
                //
                if (m * n == 0)
                {
                    return;
                }

                //
                // Try optimized code
                //
                if (ablasf.rmatrixgemmf(m, n, k, alpha, a, ia, ja, optypea, b, ib, jb, optypeb, beta, ref c, ic, jc))
                {
                    return;
                }

                //
                // if K=0, then C=Beta*C
                //
                if (k == 0)
                {
                    if ((double)(beta) != (double)(1))
                    {
                        if ((double)(beta) != (double)(0))
                        {
                            for (i = 0; i <= m - 1; i++)
                            {
                                for (j = 0; j <= n - 1; j++)
                                {
                                    c[ic + i, jc + j] = beta * c[ic + i, jc + j];
                                }
                            }
                        }
                        else
                        {
                            for (i = 0; i <= m - 1; i++)
                            {
                                for (j = 0; j <= n - 1; j++)
                                {
                                    c[ic + i, jc + j] = 0;
                                }
                            }
                        }
                    }
                    return;
                }

                //
                // General case
                //
                if (optypea == 0 & optypeb != 0)
                {

                    //
                    // A*B'
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        for (j = 0; j <= n - 1; j++)
                        {
                            if (k == 0 | (double)(alpha) == (double)(0))
                            {
                                v = 0;
                            }
                            else
                            {
                                i1_ = (jb) - (ja);
                                v = 0.0;
                                for (i_ = ja; i_ <= ja + k - 1; i_++)
                                {
                                    v += a[ia + i, i_] * b[ib + j, i_ + i1_];
                                }
                            }
                            if ((double)(beta) == (double)(0))
                            {
                                c[ic + i, jc + j] = alpha * v;
                            }
                            else
                            {
                                c[ic + i, jc + j] = beta * c[ic + i, jc + j] + alpha * v;
                            }
                        }
                    }
                    return;
                }
                if (optypea == 0 & optypeb == 0)
                {

                    //
                    // A*B
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        if ((double)(beta) != (double)(0))
                        {
                            for (i_ = jc; i_ <= jc + n - 1; i_++)
                            {
                                c[ic + i, i_] = beta * c[ic + i, i_];
                            }
                        }
                        else
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                c[ic + i, jc + j] = 0;
                            }
                        }
                        if ((double)(alpha) != (double)(0))
                        {
                            for (j = 0; j <= k - 1; j++)
                            {
                                v = alpha * a[ia + i, ja + j];
                                i1_ = (jb) - (jc);
                                for (i_ = jc; i_ <= jc + n - 1; i_++)
                                {
                                    c[ic + i, i_] = c[ic + i, i_] + v * b[ib + j, i_ + i1_];
                                }
                            }
                        }
                    }
                    return;
                }
                if (optypea != 0 & optypeb != 0)
                {

                    //
                    // A'*B'
                    //
                    for (i = 0; i <= m - 1; i++)
                    {
                        for (j = 0; j <= n - 1; j++)
                        {
                            if ((double)(alpha) == (double)(0))
                            {
                                v = 0;
                            }
                            else
                            {
                                i1_ = (jb) - (ia);
                                v = 0.0;
                                for (i_ = ia; i_ <= ia + k - 1; i_++)
                                {
                                    v += a[i_, ja + i] * b[ib + j, i_ + i1_];
                                }
                            }
                            if ((double)(beta) == (double)(0))
                            {
                                c[ic + i, jc + j] = alpha * v;
                            }
                            else
                            {
                                c[ic + i, jc + j] = beta * c[ic + i, jc + j] + alpha * v;
                            }
                        }
                    }
                    return;
                }
                if (optypea != 0 & optypeb == 0)
                {

                    //
                    // A'*B
                    //
                    if ((double)(beta) == (double)(0))
                    {
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = 0; j <= n - 1; j++)
                            {
                                c[ic + i, jc + j] = 0;
                            }
                        }
                    }
                    else
                    {
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (i_ = jc; i_ <= jc + n - 1; i_++)
                            {
                                c[ic + i, i_] = beta * c[ic + i, i_];
                            }
                        }
                    }
                    if ((double)(alpha) != (double)(0))
                    {
                        for (j = 0; j <= k - 1; j++)
                        {
                            for (i = 0; i <= m - 1; i++)
                            {
                                v = alpha * a[ia + j, ja + i];
                                i1_ = (jb) - (jc);
                                for (i_ = jc; i_ <= jc + n - 1; i_++)
                                {
                                    c[ic + i, i_] = c[ic + i, i_] + v * b[ib + j, i_ + i1_];
                                }
                            }
                        }
                    }
                    return;
                }
            }


        }
        public class ablasf
        {
            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool cmatrixrank1f(int m,
                int n,
                ref complex[,] a,
                int ia,
                int ja,
                ref complex[] u,
                int iu,
                ref complex[] v,
                int iv)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool rmatrixrank1f(int m,
                int n,
                ref double[,] a,
                int ia,
                int ja,
                ref double[] u,
                int iu,
                ref double[] v,
                int iv)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool cmatrixmvf(int m,
                int n,
                complex[,] a,
                int ia,
                int ja,
                int opa,
                complex[] x,
                int ix,
                ref complex[] y,
                int iy)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool rmatrixmvf(int m,
                int n,
                double[,] a,
                int ia,
                int ja,
                int opa,
                double[] x,
                int ix,
                ref double[] y,
                int iy)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool cmatrixrighttrsmf(int m,
                int n,
                complex[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref complex[,] x,
                int i2,
                int j2)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool cmatrixlefttrsmf(int m,
                int n,
                complex[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref complex[,] x,
                int i2,
                int j2)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool rmatrixrighttrsmf(int m,
                int n,
                double[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref double[,] x,
                int i2,
                int j2)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool rmatrixlefttrsmf(int m,
                int n,
                double[,] a,
                int i1,
                int j1,
                bool isupper,
                bool isunit,
                int optype,
                ref double[,] x,
                int i2,
                int j2)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool cmatrixsyrkf(int n,
                int k,
                double alpha,
                complex[,] a,
                int ia,
                int ja,
                int optypea,
                double beta,
                ref complex[,] c,
                int ic,
                int jc,
                bool isupper)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool rmatrixsyrkf(int n,
                int k,
                double alpha,
                double[,] a,
                int ia,
                int ja,
                int optypea,
                double beta,
                ref double[,] c,
                int ic,
                int jc,
                bool isupper)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool rmatrixgemmf(int m,
                int n,
                int k,
                double alpha,
                double[,] a,
                int ia,
                int ja,
                int optypea,
                double[,] b,
                int ib,
                int jb,
                int optypeb,
                double beta,
                ref double[,] c,
                int ic,
                int jc)
            {
                bool result = new bool();

                result = false;
                return result;
            }


            /*************************************************************************
            Fast kernel

              -- ALGLIB routine --
                 19.01.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static bool cmatrixgemmf(int m,
                int n,
                int k,
                complex alpha,
                complex[,] a,
                int ia,
                int ja,
                int optypea,
                complex[,] b,
                int ib,
                int jb,
                int optypeb,
                complex beta,
                ref complex[,] c,
                int ic,
                int jc)
            {
                bool result = new bool();

                result = false;
                return result;
            }


        }
        public class svd
        {
            /*************************************************************************
            Singular value decomposition of a rectangular matrix.

            The algorithm calculates the singular value decomposition of a matrix of
            size MxN: A = U * S * V^T

            The algorithm finds the singular values and, optionally, matrices U and V^T.
            The algorithm can find both first min(M,N) columns of matrix U and rows of
            matrix V^T (singular vectors), and matrices U and V^T wholly (of sizes MxM
            and NxN respectively).

            Take into account that the subroutine does not return matrix V but V^T.

            Input parameters:
                A           -   matrix to be decomposed.
                                Array whose indexes range within [0..M-1, 0..N-1].
                M           -   number of rows in matrix A.
                N           -   number of columns in matrix A.
                UNeeded     -   0, 1 or 2. See the description of the parameter U.
                VTNeeded    -   0, 1 or 2. See the description of the parameter VT.
                AdditionalMemory -
                                If the parameter:
                                 * equals 0, the algorithm doesn’t use additional
                                   memory (lower requirements, lower performance).
                                 * equals 1, the algorithm uses additional
                                   memory of size min(M,N)*min(M,N) of real numbers.
                                   It often speeds up the algorithm.
                                 * equals 2, the algorithm uses additional
                                   memory of size M*min(M,N) of real numbers.
                                   It allows to get a maximum performance.
                                The recommended value of the parameter is 2.

            Output parameters:
                W           -   contains singular values in descending order.
                U           -   if UNeeded=0, U isn't changed, the left singular vectors
                                are not calculated.
                                if Uneeded=1, U contains left singular vectors (first
                                min(M,N) columns of matrix U). Array whose indexes range
                                within [0..M-1, 0..Min(M,N)-1].
                                if UNeeded=2, U contains matrix U wholly. Array whose
                                indexes range within [0..M-1, 0..M-1].
                VT          -   if VTNeeded=0, VT isn’t changed, the right singular vectors
                                are not calculated.
                                if VTNeeded=1, VT contains right singular vectors (first
                                min(M,N) rows of matrix V^T). Array whose indexes range
                                within [0..min(M,N)-1, 0..N-1].
                                if VTNeeded=2, VT contains matrix V^T wholly. Array whose
                                indexes range within [0..N-1, 0..N-1].

              -- ALGLIB --
                 Copyright 2005 by Bochkanov Sergey
            *************************************************************************/
            public static bool rmatrixsvd(double[,] a,
                int m,
                int n,
                int uneeded,
                int vtneeded,
                int additionalmemory,
                ref double[] w,
                ref double[,] u,
                ref double[,] vt)
            {
                bool result = new bool();
                double[] tauq = new double[0];
                double[] taup = new double[0];
                double[] tau = new double[0];
                double[] e = new double[0];
                double[] work = new double[0];
                double[,] t2 = new double[0, 0];
                bool isupper = new bool();
                int minmn = 0;
                int ncu = 0;
                int nrvt = 0;
                int nru = 0;
                int ncvt = 0;
                int i = 0;
                int j = 0;

                a = (double[,])a.Clone();
                w = new double[0];
                u = new double[0, 0];
                vt = new double[0, 0];

                result = true;
                if (m == 0 | n == 0)
                {
                    return result;
                }
                ap.assert(uneeded >= 0 & uneeded <= 2, "SVDDecomposition: wrong parameters!");
                ap.assert(vtneeded >= 0 & vtneeded <= 2, "SVDDecomposition: wrong parameters!");
                ap.assert(additionalmemory >= 0 & additionalmemory <= 2, "SVDDecomposition: wrong parameters!");

                //
                // initialize
                //
                minmn = Math.Min(m, n);
                w = new double[minmn + 1];
                ncu = 0;
                nru = 0;
                if (uneeded == 1)
                {
                    nru = m;
                    ncu = minmn;
                    u = new double[nru - 1 + 1, ncu - 1 + 1];
                }
                if (uneeded == 2)
                {
                    nru = m;
                    ncu = m;
                    u = new double[nru - 1 + 1, ncu - 1 + 1];
                }
                nrvt = 0;
                ncvt = 0;
                if (vtneeded == 1)
                {
                    nrvt = minmn;
                    ncvt = n;
                    vt = new double[nrvt - 1 + 1, ncvt - 1 + 1];
                }
                if (vtneeded == 2)
                {
                    nrvt = n;
                    ncvt = n;
                    vt = new double[nrvt - 1 + 1, ncvt - 1 + 1];
                }

                //
                // M much larger than N
                // Use bidiagonal reduction with QR-decomposition
                //
                if ((double)(m) > (double)(1.6 * n))
                {
                    if (uneeded == 0)
                    {

                        //
                        // No left singular vectors to be computed
                        //
                        ortfac.rmatrixqr(ref a, m, n, ref tau);
                        for (i = 0; i <= n - 1; i++)
                        {
                            for (j = 0; j <= i - 1; j++)
                            {
                                a[i, j] = 0;
                            }
                        }
                        ortfac.rmatrixbd(ref a, n, n, ref tauq, ref taup);
                        ortfac.rmatrixbdunpackpt(a, n, n, taup, nrvt, ref vt);
                        ortfac.rmatrixbdunpackdiagonals(a, n, n, ref isupper, ref w, ref e);
                        result = bdsvd.rmatrixbdsvd(ref w, e, n, isupper, false, ref u, 0, ref a, 0, ref vt, ncvt);
                        return result;
                    }
                    else
                    {

                        //
                        // Left singular vectors (may be full matrix U) to be computed
                        //
                        ortfac.rmatrixqr(ref a, m, n, ref tau);
                        ortfac.rmatrixqrunpackq(a, m, n, tau, ncu, ref u);
                        for (i = 0; i <= n - 1; i++)
                        {
                            for (j = 0; j <= i - 1; j++)
                            {
                                a[i, j] = 0;
                            }
                        }
                        ortfac.rmatrixbd(ref a, n, n, ref tauq, ref taup);
                        ortfac.rmatrixbdunpackpt(a, n, n, taup, nrvt, ref vt);
                        ortfac.rmatrixbdunpackdiagonals(a, n, n, ref isupper, ref w, ref e);
                        if (additionalmemory < 1)
                        {

                            //
                            // No additional memory can be used
                            //
                            ortfac.rmatrixbdmultiplybyq(a, n, n, tauq, ref u, m, n, true, false);
                            result = bdsvd.rmatrixbdsvd(ref w, e, n, isupper, false, ref u, m, ref a, 0, ref vt, ncvt);
                        }
                        else
                        {

                            //
                            // Large U. Transforming intermediate matrix T2
                            //
                            work = new double[Math.Max(m, n) + 1];
                            ortfac.rmatrixbdunpackq(a, n, n, tauq, n, ref t2);
                            blas.copymatrix(u, 0, m - 1, 0, n - 1, ref a, 0, m - 1, 0, n - 1);
                            blas.inplacetranspose(ref t2, 0, n - 1, 0, n - 1, ref work);
                            result = bdsvd.rmatrixbdsvd(ref w, e, n, isupper, false, ref u, 0, ref t2, n, ref vt, ncvt);
                            blas.matrixmatrixmultiply(a, 0, m - 1, 0, n - 1, false, t2, 0, n - 1, 0, n - 1, true, 1.0, ref u, 0, m - 1, 0, n - 1, 0.0, ref work);
                        }
                        return result;
                    }
                }

                //
                // N much larger than M
                // Use bidiagonal reduction with LQ-decomposition
                //
                if ((double)(n) > (double)(1.6 * m))
                {
                    if (vtneeded == 0)
                    {

                        //
                        // No right singular vectors to be computed
                        //
                        ortfac.rmatrixlq(ref a, m, n, ref tau);
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = i + 1; j <= m - 1; j++)
                            {
                                a[i, j] = 0;
                            }
                        }
                        ortfac.rmatrixbd(ref a, m, m, ref tauq, ref taup);
                        ortfac.rmatrixbdunpackq(a, m, m, tauq, ncu, ref u);
                        ortfac.rmatrixbdunpackdiagonals(a, m, m, ref isupper, ref w, ref e);
                        work = new double[m + 1];
                        blas.inplacetranspose(ref u, 0, nru - 1, 0, ncu - 1, ref work);
                        result = bdsvd.rmatrixbdsvd(ref w, e, m, isupper, false, ref a, 0, ref u, nru, ref vt, 0);
                        blas.inplacetranspose(ref u, 0, nru - 1, 0, ncu - 1, ref work);
                        return result;
                    }
                    else
                    {

                        //
                        // Right singular vectors (may be full matrix VT) to be computed
                        //
                        ortfac.rmatrixlq(ref a, m, n, ref tau);
                        ortfac.rmatrixlqunpackq(a, m, n, tau, nrvt, ref vt);
                        for (i = 0; i <= m - 1; i++)
                        {
                            for (j = i + 1; j <= m - 1; j++)
                            {
                                a[i, j] = 0;
                            }
                        }
                        ortfac.rmatrixbd(ref a, m, m, ref tauq, ref taup);
                        ortfac.rmatrixbdunpackq(a, m, m, tauq, ncu, ref u);
                        ortfac.rmatrixbdunpackdiagonals(a, m, m, ref isupper, ref w, ref e);
                        work = new double[Math.Max(m, n) + 1];
                        blas.inplacetranspose(ref u, 0, nru - 1, 0, ncu - 1, ref work);
                        if (additionalmemory < 1)
                        {

                            //
                            // No additional memory available
                            //
                            ortfac.rmatrixbdmultiplybyp(a, m, m, taup, ref vt, m, n, false, true);
                            result = bdsvd.rmatrixbdsvd(ref w, e, m, isupper, false, ref a, 0, ref u, nru, ref vt, n);
                        }
                        else
                        {

                            //
                            // Large VT. Transforming intermediate matrix T2
                            //
                            ortfac.rmatrixbdunpackpt(a, m, m, taup, m, ref t2);
                            result = bdsvd.rmatrixbdsvd(ref w, e, m, isupper, false, ref a, 0, ref u, nru, ref t2, m);
                            blas.copymatrix(vt, 0, m - 1, 0, n - 1, ref a, 0, m - 1, 0, n - 1);
                            blas.matrixmatrixmultiply(t2, 0, m - 1, 0, m - 1, false, a, 0, m - 1, 0, n - 1, false, 1.0, ref vt, 0, m - 1, 0, n - 1, 0.0, ref work);
                        }
                        blas.inplacetranspose(ref u, 0, nru - 1, 0, ncu - 1, ref work);
                        return result;
                    }
                }

                //
                // M<=N
                // We can use inplace transposition of U to get rid of columnwise operations
                //
                if (m <= n)
                {
                    ortfac.rmatrixbd(ref a, m, n, ref tauq, ref taup);
                    ortfac.rmatrixbdunpackq(a, m, n, tauq, ncu, ref u);
                    ortfac.rmatrixbdunpackpt(a, m, n, taup, nrvt, ref vt);
                    ortfac.rmatrixbdunpackdiagonals(a, m, n, ref isupper, ref w, ref e);
                    work = new double[m + 1];
                    blas.inplacetranspose(ref u, 0, nru - 1, 0, ncu - 1, ref work);
                    result = bdsvd.rmatrixbdsvd(ref w, e, minmn, isupper, false, ref a, 0, ref u, nru, ref vt, ncvt);
                    blas.inplacetranspose(ref u, 0, nru - 1, 0, ncu - 1, ref work);
                   
                    return result;
                }

                //
                // Simple bidiagonal reduction
                //
                ortfac.rmatrixbd(ref a, m, n, ref tauq, ref taup);
                ortfac.rmatrixbdunpackq(a, m, n, tauq, ncu, ref u);
                ortfac.rmatrixbdunpackpt(a, m, n, taup, nrvt, ref vt);
                ortfac.rmatrixbdunpackdiagonals(a, m, n, ref isupper, ref w, ref e);
                if (additionalmemory < 2 | uneeded == 0)
                {

                    //
                    // We cant use additional memory or there is no need in such operations
                    //
                    result = bdsvd.rmatrixbdsvd(ref w, e, minmn, isupper, false, ref u, nru, ref a, 0, ref vt, ncvt);
                }
                else
                {

                    //
                    // We can use additional memory
                    //
                    t2 = new double[minmn - 1 + 1, m - 1 + 1];
                    blas.copyandtranspose(u, 0, m - 1, 0, minmn - 1, ref t2, 0, minmn - 1, 0, m - 1);
                    result = bdsvd.rmatrixbdsvd(ref w, e, minmn, isupper, false, ref u, 0, ref t2, m, ref vt, ncvt);
                    blas.copyandtranspose(t2, 0, minmn - 1, 0, m - 1, ref u, 0, m - 1, 0, minmn - 1);
                }
                return result;
            }


        }
        public class bdsvd
        {
            /*************************************************************************
            Singular value decomposition of a bidiagonal matrix (extended algorithm)

            The algorithm performs the singular value decomposition  of  a  bidiagonal
            matrix B (upper or lower) representing it as B = Q*S*P^T, where Q and  P -
            orthogonal matrices, S - diagonal matrix with non-negative elements on the
            main diagonal, in descending order.

            The  algorithm  finds  singular  values.  In  addition,  the algorithm can
            calculate  matrices  Q  and P (more precisely, not the matrices, but their
            product  with  given  matrices U and VT - U*Q and (P^T)*VT)).  Of  course,
            matrices U and VT can be of any type, including identity. Furthermore, the
            algorithm can calculate Q'*C (this product is calculated more  effectively
            than U*Q,  because  this calculation operates with rows instead  of matrix
            columns).

            The feature of the algorithm is its ability to find  all  singular  values
            including those which are arbitrarily close to 0  with  relative  accuracy
            close to  machine precision. If the parameter IsFractionalAccuracyRequired
            is set to True, all singular values will have high relative accuracy close
            to machine precision. If the parameter is set to False, only  the  biggest
            singular value will have relative accuracy  close  to  machine  precision.
            The absolute error of other singular values is equal to the absolute error
            of the biggest singular value.

            Input parameters:
                D       -   main diagonal of matrix B.
                            Array whose index ranges within [0..N-1].
                E       -   superdiagonal (or subdiagonal) of matrix B.
                            Array whose index ranges within [0..N-2].
                N       -   size of matrix B.
                IsUpper -   True, if the matrix is upper bidiagonal.
                IsFractionalAccuracyRequired -
                            accuracy to search singular values with.
                U       -   matrix to be multiplied by Q.
                            Array whose indexes range within [0..NRU-1, 0..N-1].
                            The matrix can be bigger, in that case only the  submatrix
                            [0..NRU-1, 0..N-1] will be multiplied by Q.
                NRU     -   number of rows in matrix U.
                C       -   matrix to be multiplied by Q'.
                            Array whose indexes range within [0..N-1, 0..NCC-1].
                            The matrix can be bigger, in that case only the  submatrix
                            [0..N-1, 0..NCC-1] will be multiplied by Q'.
                NCC     -   number of columns in matrix C.
                VT      -   matrix to be multiplied by P^T.
                            Array whose indexes range within [0..N-1, 0..NCVT-1].
                            The matrix can be bigger, in that case only the  submatrix
                            [0..N-1, 0..NCVT-1] will be multiplied by P^T.
                NCVT    -   number of columns in matrix VT.

            Output parameters:
                D       -   singular values of matrix B in descending order.
                U       -   if NRU>0, contains matrix U*Q.
                VT      -   if NCVT>0, contains matrix (P^T)*VT.
                C       -   if NCC>0, contains matrix Q'*C.

            Result:
                True, if the algorithm has converged.
                False, if the algorithm hasn't converged (rare case).

            Additional information:
                The type of convergence is controlled by the internal  parameter  TOL.
                If the parameter is greater than 0, the singular values will have
                relative accuracy TOL. If TOL<0, the singular values will have
                absolute accuracy ABS(TOL)*norm(B).
                By default, |TOL| falls within the range of 10*Epsilon and 100*Epsilon,
                where Epsilon is the machine precision. It is not  recommended  to  use
                TOL less than 10*Epsilon since this will  considerably  slow  down  the
                algorithm and may not lead to error decreasing.
            History:
                * 31 March, 2007.
                    changed MAXITR from 6 to 12.

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 October 31, 1999.
            *************************************************************************/
            public static bool rmatrixbdsvd(ref double[] d,
                double[] e,
                int n,
                bool isupper,
                bool isfractionalaccuracyrequired,
                ref double[,] u,
                int nru,
                ref double[,] c,
                int ncc,
                ref double[,] vt,
                int ncvt)
            {
                bool result = new bool();
                double[] d1 = new double[0];
                double[] e1 = new double[0];
                int i_ = 0;
                int i1_ = 0;

                e = (double[])e.Clone();

                d1 = new double[n + 1];
                i1_ = (0) - (1);
                for (i_ = 1; i_ <= n; i_++)
                {
                    d1[i_] = d[i_ + i1_];
                }
                if (n > 1)
                {
                    e1 = new double[n - 1 + 1];
                    i1_ = (0) - (1);
                    for (i_ = 1; i_ <= n - 1; i_++)
                    {
                        e1[i_] = e[i_ + i1_];
                    }
                }
                result = bidiagonalsvddecompositioninternal(ref d1, e1, n, isupper, isfractionalaccuracyrequired, ref u, 0, nru, ref c, 0, ncc, ref vt, 0, ncvt);
                i1_ = (1) - (0);
                for (i_ = 0; i_ <= n - 1; i_++)
                {
                    d[i_] = d1[i_ + i1_];
                }
                return result;
            }


            public static bool bidiagonalsvddecomposition(ref double[] d,
                double[] e,
                int n,
                bool isupper,
                bool isfractionalaccuracyrequired,
                ref double[,] u,
                int nru,
                ref double[,] c,
                int ncc,
                ref double[,] vt,
                int ncvt)
            {
                bool result = new bool();

                e = (double[])e.Clone();

                result = bidiagonalsvddecompositioninternal(ref d, e, n, isupper, isfractionalaccuracyrequired, ref u, 1, nru, ref c, 1, ncc, ref vt, 1, ncvt);
                return result;
            }


            /*************************************************************************
            Internal working subroutine for bidiagonal decomposition
            *************************************************************************/
            private static bool bidiagonalsvddecompositioninternal(ref double[] d,
                double[] e,
                int n,
                bool isupper,
                bool isfractionalaccuracyrequired,
                ref double[,] u,
                int ustart,
                int nru,
                ref double[,] c,
                int cstart,
                int ncc,
                ref double[,] vt,
                int vstart,
                int ncvt)
            {
                bool result = new bool();
                int i = 0;
                int idir = 0;
                int isub = 0;
                int iter = 0;
                int j = 0;
                int ll = 0;
                int lll = 0;
                int m = 0;
                int maxit = 0;
                int oldll = 0;
                int oldm = 0;
                double abse = 0;
                double abss = 0;
                double cosl = 0;
                double cosr = 0;
                double cs = 0;
                double eps = 0;
                double f = 0;
                double g = 0;
                double h = 0;
                double mu = 0;
                double oldcs = 0;
                double oldsn = 0;
                double r = 0;
                double shift = 0;
                double sigmn = 0;
                double sigmx = 0;
                double sinl = 0;
                double sinr = 0;
                double sll = 0;
                double smax = 0;
                double smin = 0;
                double sminl = 0;
                double sminlo = 0;
                double sminoa = 0;
                double sn = 0;
                double thresh = 0;
                double tol = 0;
                double tolmul = 0;
                double unfl = 0;
                double[] work0 = new double[0];
                double[] work1 = new double[0];
                double[] work2 = new double[0];
                double[] work3 = new double[0];
                int maxitr = 0;
                bool matrixsplitflag = new bool();
                bool iterflag = new bool();
                double[] utemp = new double[0];
                double[] vttemp = new double[0];
                double[] ctemp = new double[0];
                double[] etemp = new double[0];
                //bool rightside = new bool();
                bool fwddir = new bool();
                double tmp = 0;
                int mm1 = 0;
                int mm0 = 0;
                bool bchangedir = new bool();
                int uend = 0;
                int cend = 0;
                int vend = 0;
                int i_ = 0;

                e = (double[])e.Clone();

                result = true;
                if (n == 0)
                {
                    return result;
                }
                if (n == 1)
                {
                    if ((double)(d[1]) < (double)(0))
                    {
                        d[1] = -d[1];
                        if (ncvt > 0)
                        {
                            for (i_ = vstart; i_ <= vstart + ncvt - 1; i_++)
                            {
                                vt[vstart, i_] = -1 * vt[vstart, i_];
                            }
                        }
                    }
                    return result;
                }

                //
                // these initializers are not really necessary,
                // but without them compiler complains about uninitialized locals
                //
                ll = 0;
                oldsn = 0;

                //
                // init
                //
                work0 = new double[n - 1 + 1];
                work1 = new double[n - 1 + 1];
                work2 = new double[n - 1 + 1];
                work3 = new double[n - 1 + 1];
                uend = ustart + Math.Max(nru - 1, 0);
                vend = vstart + Math.Max(ncvt - 1, 0);
                cend = cstart + Math.Max(ncc - 1, 0);
                utemp = new double[uend + 1];
                vttemp = new double[vend + 1];
                ctemp = new double[cend + 1];
                maxitr = 12;
                //rightside = true;
                fwddir = true;

                //
                // resize E from N-1 to N
                //
                etemp = new double[n + 1];
                for (i = 1; i <= n - 1; i++)
                {
                    etemp[i] = e[i];
                }
                e = new double[n + 1];
                for (i = 1; i <= n - 1; i++)
                {
                    e[i] = etemp[i];
                }
                e[n] = 0;
                idir = 0;

                //
                // Get machine constants
                //
                eps = math.machineepsilon;
                unfl = math.minrealnumber;

                //
                // If matrix lower bidiagonal, rotate to be upper bidiagonal
                // by applying Givens rotations on the left
                //
                if (!isupper)
                {
                    for (i = 1; i <= n - 1; i++)
                    {
                        rotations.generaterotation(d[i], e[i], ref cs, ref sn, ref r);
                        d[i] = r;
                        e[i] = sn * d[i + 1];
                        d[i + 1] = cs * d[i + 1];
                        work0[i] = cs;
                        work1[i] = sn;
                    }

                    //
                    // Update singular vectors if desired
                    //
                    if (nru > 0)
                    {
                        rotations.applyrotationsfromtheright(fwddir, ustart, uend, 1 + ustart - 1, n + ustart - 1, work0, work1, ref u, ref utemp);
                    }
                    if (ncc > 0)
                    {
                        rotations.applyrotationsfromtheleft(fwddir, 1 + cstart - 1, n + cstart - 1, cstart, cend, work0, work1, ref c, ref ctemp);
                    }
                }

                //
                // Compute singular values to relative accuracy TOL
                // (By setting TOL to be negative, algorithm will compute
                // singular values to absolute accuracy ABS(TOL)*norm(input matrix))
                //
                tolmul = Math.Max(10, Math.Min(100, Math.Pow(eps, -0.125)));
                tol = tolmul * eps;
                if (!isfractionalaccuracyrequired)
                {
                    tol = -tol;
                }

                //
                // Compute approximate maximum, minimum singular values
                //
                smax = 0;
                for (i = 1; i <= n; i++)
                {
                    smax = Math.Max(smax, Math.Abs(d[i]));
                }
                for (i = 1; i <= n - 1; i++)
                {
                    smax = Math.Max(smax, Math.Abs(e[i]));
                }
                sminl = 0;
                if ((double)(tol) >= (double)(0))
                {

                    //
                    // Relative accuracy desired
                    //
                    sminoa = Math.Abs(d[1]);
                    if ((double)(sminoa) != (double)(0))
                    {
                        mu = sminoa;
                        for (i = 2; i <= n; i++)
                        {
                            mu = Math.Abs(d[i]) * (mu / (mu + Math.Abs(e[i - 1])));
                            sminoa = Math.Min(sminoa, mu);
                            if ((double)(sminoa) == (double)(0))
                            {
                                break;
                            }
                        }
                    }
                    sminoa = sminoa / Math.Sqrt(n);
                    thresh = Math.Max(tol * sminoa, maxitr * n * n * unfl);
                }
                else
                {

                    //
                    // Absolute accuracy desired
                    //
                    thresh = Math.Max(Math.Abs(tol) * smax, maxitr * n * n * unfl);
                }

                //
                // Prepare for main iteration loop for the singular values
                // (MAXIT is the maximum number of passes through the inner
                // loop permitted before nonconvergence signalled.)
                //
                maxit = maxitr * n * n;
                iter = 0;
                oldll = -1;
                oldm = -1;

                //
                // M points to last element of unconverged part of matrix
                //
                m = n;

                //
                // Begin main iteration loop
                //
                while (true)
                {

                    //
                    // Check for convergence or exceeding iteration count
                    //
                    if (m <= 1)
                    {
                        break;
                    }
                    if (iter > maxit)
                    {
                        result = false;
                        return result;
                    }

                    //
                    // Find diagonal block of matrix to work on
                    //
                    if ((double)(tol) < (double)(0) & (double)(Math.Abs(d[m])) <= (double)(thresh))
                    {
                        d[m] = 0;
                    }
                    smax = Math.Abs(d[m]);
                    smin = smax;
                    matrixsplitflag = false;
                    for (lll = 1; lll <= m - 1; lll++)
                    {
                        ll = m - lll;
                        abss = Math.Abs(d[ll]);
                        abse = Math.Abs(e[ll]);
                        if ((double)(tol) < (double)(0) & (double)(abss) <= (double)(thresh))
                        {
                            d[ll] = 0;
                        }
                        if ((double)(abse) <= (double)(thresh))
                        {
                            matrixsplitflag = true;
                            break;
                        }
                        smin = Math.Min(smin, abss);
                        smax = Math.Max(smax, Math.Max(abss, abse));
                    }
                    if (!matrixsplitflag)
                    {
                        ll = 0;
                    }
                    else
                    {

                        //
                        // Matrix splits since E(LL) = 0
                        //
                        e[ll] = 0;
                        if (ll == m - 1)
                        {

                            //
                            // Convergence of bottom singular value, return to top of loop
                            //
                            m = m - 1;
                            continue;
                        }
                    }
                    ll = ll + 1;

                    //
                    // E(LL) through E(M-1) are nonzero, E(LL-1) is zero
                    //
                    if (ll == m - 1)
                    {

                        //
                        // 2 by 2 block, handle separately
                        //
                        svdv2x2(d[m - 1], e[m - 1], d[m], ref sigmn, ref sigmx, ref sinr, ref cosr, ref sinl, ref cosl);
                        d[m - 1] = sigmx;
                        e[m - 1] = 0;
                        d[m] = sigmn;

                        //
                        // Compute singular vectors, if desired
                        //
                        if (ncvt > 0)
                        {
                            mm0 = m + (vstart - 1);
                            mm1 = m - 1 + (vstart - 1);
                            for (i_ = vstart; i_ <= vend; i_++)
                            {
                                vttemp[i_] = cosr * vt[mm1, i_];
                            }
                            for (i_ = vstart; i_ <= vend; i_++)
                            {
                                vttemp[i_] = vttemp[i_] + sinr * vt[mm0, i_];
                            }
                            for (i_ = vstart; i_ <= vend; i_++)
                            {
                                vt[mm0, i_] = cosr * vt[mm0, i_];
                            }
                            for (i_ = vstart; i_ <= vend; i_++)
                            {
                                vt[mm0, i_] = vt[mm0, i_] - sinr * vt[mm1, i_];
                            }
                            for (i_ = vstart; i_ <= vend; i_++)
                            {
                                vt[mm1, i_] = vttemp[i_];
                            }
                        }
                        if (nru > 0)
                        {
                            mm0 = m + ustart - 1;
                            mm1 = m - 1 + ustart - 1;
                            for (i_ = ustart; i_ <= uend; i_++)
                            {
                                utemp[i_] = cosl * u[i_, mm1];
                            }
                            for (i_ = ustart; i_ <= uend; i_++)
                            {
                                utemp[i_] = utemp[i_] + sinl * u[i_, mm0];
                            }
                            for (i_ = ustart; i_ <= uend; i_++)
                            {
                                u[i_, mm0] = cosl * u[i_, mm0];
                            }
                            for (i_ = ustart; i_ <= uend; i_++)
                            {
                                u[i_, mm0] = u[i_, mm0] - sinl * u[i_, mm1];
                            }
                            for (i_ = ustart; i_ <= uend; i_++)
                            {
                                u[i_, mm1] = utemp[i_];
                            }
                        }
                        if (ncc > 0)
                        {
                            mm0 = m + cstart - 1;
                            mm1 = m - 1 + cstart - 1;
                            for (i_ = cstart; i_ <= cend; i_++)
                            {
                                ctemp[i_] = cosl * c[mm1, i_];
                            }
                            for (i_ = cstart; i_ <= cend; i_++)
                            {
                                ctemp[i_] = ctemp[i_] + sinl * c[mm0, i_];
                            }
                            for (i_ = cstart; i_ <= cend; i_++)
                            {
                                c[mm0, i_] = cosl * c[mm0, i_];
                            }
                            for (i_ = cstart; i_ <= cend; i_++)
                            {
                                c[mm0, i_] = c[mm0, i_] - sinl * c[mm1, i_];
                            }
                            for (i_ = cstart; i_ <= cend; i_++)
                            {
                                c[mm1, i_] = ctemp[i_];
                            }
                        }
                        m = m - 2;
                        continue;
                    }

                    //
                    // If working on new submatrix, choose shift direction
                    // (from larger end diagonal element towards smaller)
                    //
                    // Previously was
                    //     "if (LL>OLDM) or (M<OLDLL) then"
                    // fixed thanks to Michael Rolle < m@rolle.name >
                    // Very strange that LAPACK still contains it.
                    //
                    bchangedir = false;
                    if (idir == 1 & (double)(Math.Abs(d[ll])) < (double)(1.0E-3 * Math.Abs(d[m])))
                    {
                        bchangedir = true;
                    }
                    if (idir == 2 & (double)(Math.Abs(d[m])) < (double)(1.0E-3 * Math.Abs(d[ll])))
                    {
                        bchangedir = true;
                    }
                    if ((ll != oldll | m != oldm) | bchangedir)
                    {
                        if ((double)(Math.Abs(d[ll])) >= (double)(Math.Abs(d[m])))
                        {

                            //
                            // Chase bulge from top (big end) to bottom (small end)
                            //
                            idir = 1;
                        }
                        else
                        {

                            //
                            // Chase bulge from bottom (big end) to top (small end)
                            //
                            idir = 2;
                        }
                    }

                    //
                    // Apply convergence tests
                    //
                    if (idir == 1)
                    {

                        //
                        // Run convergence test in forward direction
                        // First apply standard test to bottom of matrix
                        //
                        if ((double)(Math.Abs(e[m - 1])) <= (double)(Math.Abs(tol) * Math.Abs(d[m])) | ((double)(tol) < (double)(0) & (double)(Math.Abs(e[m - 1])) <= (double)(thresh)))
                        {
                            e[m - 1] = 0;
                            continue;
                        }
                        if ((double)(tol) >= (double)(0))
                        {

                            //
                            // If relative accuracy desired,
                            // apply convergence criterion forward
                            //
                            mu = Math.Abs(d[ll]);
                            sminl = mu;
                            iterflag = false;
                            for (lll = ll; lll <= m - 1; lll++)
                            {
                                if ((double)(Math.Abs(e[lll])) <= (double)(tol * mu))
                                {
                                    e[lll] = 0;
                                    iterflag = true;
                                    break;
                                }
                                sminlo = sminl;
                                mu = Math.Abs(d[lll + 1]) * (mu / (mu + Math.Abs(e[lll])));
                                sminl = Math.Min(sminl, mu);
                            }
                            if (iterflag)
                            {
                                continue;
                            }
                        }
                    }
                    else
                    {

                        //
                        // Run convergence test in backward direction
                        // First apply standard test to top of matrix
                        //
                        if ((double)(Math.Abs(e[ll])) <= (double)(Math.Abs(tol) * Math.Abs(d[ll])) | ((double)(tol) < (double)(0) & (double)(Math.Abs(e[ll])) <= (double)(thresh)))
                        {
                            e[ll] = 0;
                            continue;
                        }
                        if ((double)(tol) >= (double)(0))
                        {

                            //
                            // If relative accuracy desired,
                            // apply convergence criterion backward
                            //
                            mu = Math.Abs(d[m]);
                            sminl = mu;
                            iterflag = false;
                            for (lll = m - 1; lll >= ll; lll--)
                            {
                                if ((double)(Math.Abs(e[lll])) <= (double)(tol * mu))
                                {
                                    e[lll] = 0;
                                    iterflag = true;
                                    break;
                                }
                                sminlo = sminl;
                                mu = Math.Abs(d[lll]) * (mu / (mu + Math.Abs(e[lll])));
                                sminl = Math.Min(sminl, mu);
                            }
                            if (iterflag)
                            {
                                continue;
                            }
                        }
                    }
                    oldll = ll;
                    oldm = m;

                    //
                    // Compute shift.  First, test if shifting would ruin relative
                    // accuracy, and if so set the shift to zero.
                    //
                    if ((double)(tol) >= (double)(0) & (double)(n * tol * (sminl / smax)) <= (double)(Math.Max(eps, 0.01 * tol)))
                    {

                        //
                        // Use a zero shift to avoid loss of relative accuracy
                        //
                        shift = 0;
                    }
                    else
                    {

                        //
                        // Compute the shift from 2-by-2 block at end of matrix
                        //
                        if (idir == 1)
                        {
                            sll = Math.Abs(d[ll]);
                            svd2x2(d[m - 1], e[m - 1], d[m], ref shift, ref r);
                        }
                        else
                        {
                            sll = Math.Abs(d[m]);
                            svd2x2(d[ll], e[ll], d[ll + 1], ref shift, ref r);
                        }

                        //
                        // Test if shift negligible, and if so set to zero
                        //
                        if ((double)(sll) > (double)(0))
                        {
                            if ((double)(math.sqr(shift / sll)) < (double)(eps))
                            {
                                shift = 0;
                            }
                        }
                    }

                    //
                    // Increment iteration count
                    //
                    iter = iter + m - ll;

                    //
                    // If SHIFT = 0, do simplified QR iteration
                    //
                    if ((double)(shift) == (double)(0))
                    {
                        if (idir == 1)
                        {

                            //
                            // Chase bulge from top to bottom
                            // Save cosines and sines for later singular vector updates
                            //
                            cs = 1;
                            oldcs = 1;
                            for (i = ll; i <= m - 1; i++)
                            {
                                rotations.generaterotation(d[i] * cs, e[i], ref cs, ref sn, ref r);
                                if (i > ll)
                                {
                                    e[i - 1] = oldsn * r;
                                }
                                rotations.generaterotation(oldcs * r, d[i + 1] * sn, ref oldcs, ref oldsn, ref tmp);
                                d[i] = tmp;
                                work0[i - ll + 1] = cs;
                                work1[i - ll + 1] = sn;
                                work2[i - ll + 1] = oldcs;
                                work3[i - ll + 1] = oldsn;
                            }
                            h = d[m] * cs;
                            d[m] = h * oldcs;
                            e[m - 1] = h * oldsn;

                            //
                            // Update singular vectors
                            //
                            if (ncvt > 0)
                            {
                                rotations.applyrotationsfromtheleft(fwddir, ll + vstart - 1, m + vstart - 1, vstart, vend, work0, work1, ref vt, ref vttemp);
                            }
                            if (nru > 0)
                            {
                                rotations.applyrotationsfromtheright(fwddir, ustart, uend, ll + ustart - 1, m + ustart - 1, work2, work3, ref u, ref utemp);
                            }
                            if (ncc > 0)
                            {
                                rotations.applyrotationsfromtheleft(fwddir, ll + cstart - 1, m + cstart - 1, cstart, cend, work2, work3, ref c, ref ctemp);
                            }

                            //
                            // Test convergence
                            //
                            if ((double)(Math.Abs(e[m - 1])) <= (double)(thresh))
                            {
                                e[m - 1] = 0;
                            }
                        }
                        else
                        {

                            //
                            // Chase bulge from bottom to top
                            // Save cosines and sines for later singular vector updates
                            //
                            cs = 1;
                            oldcs = 1;
                            for (i = m; i >= ll + 1; i--)
                            {
                                rotations.generaterotation(d[i] * cs, e[i - 1], ref cs, ref sn, ref r);
                                if (i < m)
                                {
                                    e[i] = oldsn * r;
                                }
                                rotations.generaterotation(oldcs * r, d[i - 1] * sn, ref oldcs, ref oldsn, ref tmp);
                                d[i] = tmp;
                                work0[i - ll] = cs;
                                work1[i - ll] = -sn;
                                work2[i - ll] = oldcs;
                                work3[i - ll] = -oldsn;
                            }
                            h = d[ll] * cs;
                            d[ll] = h * oldcs;
                            e[ll] = h * oldsn;

                            //
                            // Update singular vectors
                            //
                            if (ncvt > 0)
                            {
                                rotations.applyrotationsfromtheleft(!fwddir, ll + vstart - 1, m + vstart - 1, vstart, vend, work2, work3, ref vt, ref vttemp);
                            }
                            if (nru > 0)
                            {
                                rotations.applyrotationsfromtheright(!fwddir, ustart, uend, ll + ustart - 1, m + ustart - 1, work0, work1, ref u, ref utemp);
                            }
                            if (ncc > 0)
                            {
                                rotations.applyrotationsfromtheleft(!fwddir, ll + cstart - 1, m + cstart - 1, cstart, cend, work0, work1, ref c, ref ctemp);
                            }

                            //
                            // Test convergence
                            //
                            if ((double)(Math.Abs(e[ll])) <= (double)(thresh))
                            {
                                e[ll] = 0;
                            }
                        }
                    }
                    else
                    {

                        //
                        // Use nonzero shift
                        //
                        if (idir == 1)
                        {

                            //
                            // Chase bulge from top to bottom
                            // Save cosines and sines for later singular vector updates
                            //
                            f = (Math.Abs(d[ll]) - shift) * (extsignbdsqr(1, d[ll]) + shift / d[ll]);
                            g = e[ll];
                            for (i = ll; i <= m - 1; i++)
                            {
                                rotations.generaterotation(f, g, ref cosr, ref sinr, ref r);
                                if (i > ll)
                                {
                                    e[i - 1] = r;
                                }
                                f = cosr * d[i] + sinr * e[i];
                                e[i] = cosr * e[i] - sinr * d[i];
                                g = sinr * d[i + 1];
                                d[i + 1] = cosr * d[i + 1];
                                rotations.generaterotation(f, g, ref cosl, ref sinl, ref r);
                                d[i] = r;
                                f = cosl * e[i] + sinl * d[i + 1];
                                d[i + 1] = cosl * d[i + 1] - sinl * e[i];
                                if (i < m - 1)
                                {
                                    g = sinl * e[i + 1];
                                    e[i + 1] = cosl * e[i + 1];
                                }
                                work0[i - ll + 1] = cosr;
                                work1[i - ll + 1] = sinr;
                                work2[i - ll + 1] = cosl;
                                work3[i - ll + 1] = sinl;
                            }
                            e[m - 1] = f;

                            //
                            // Update singular vectors
                            //
                            if (ncvt > 0)
                            {
                                rotations.applyrotationsfromtheleft(fwddir, ll + vstart - 1, m + vstart - 1, vstart, vend, work0, work1, ref vt, ref vttemp);
                            }
                            if (nru > 0)
                            {
                                rotations.applyrotationsfromtheright(fwddir, ustart, uend, ll + ustart - 1, m + ustart - 1, work2, work3, ref u, ref utemp);
                            }
                            if (ncc > 0)
                            {
                                rotations.applyrotationsfromtheleft(fwddir, ll + cstart - 1, m + cstart - 1, cstart, cend, work2, work3, ref c, ref ctemp);
                            }

                            //
                            // Test convergence
                            //
                            if ((double)(Math.Abs(e[m - 1])) <= (double)(thresh))
                            {
                                e[m - 1] = 0;
                            }
                        }
                        else
                        {

                            //
                            // Chase bulge from bottom to top
                            // Save cosines and sines for later singular vector updates
                            //
                            f = (Math.Abs(d[m]) - shift) * (extsignbdsqr(1, d[m]) + shift / d[m]);
                            g = e[m - 1];
                            for (i = m; i >= ll + 1; i--)
                            {
                                rotations.generaterotation(f, g, ref cosr, ref sinr, ref r);
                                if (i < m)
                                {
                                    e[i] = r;
                                }
                                f = cosr * d[i] + sinr * e[i - 1];
                                e[i - 1] = cosr * e[i - 1] - sinr * d[i];
                                g = sinr * d[i - 1];
                                d[i - 1] = cosr * d[i - 1];
                                rotations.generaterotation(f, g, ref cosl, ref sinl, ref r);
                                d[i] = r;
                                f = cosl * e[i - 1] + sinl * d[i - 1];
                                d[i - 1] = cosl * d[i - 1] - sinl * e[i - 1];
                                if (i > ll + 1)
                                {
                                    g = sinl * e[i - 2];
                                    e[i - 2] = cosl * e[i - 2];
                                }
                                work0[i - ll] = cosr;
                                work1[i - ll] = -sinr;
                                work2[i - ll] = cosl;
                                work3[i - ll] = -sinl;
                            }
                            e[ll] = f;

                            //
                            // Test convergence
                            //
                            if ((double)(Math.Abs(e[ll])) <= (double)(thresh))
                            {
                                e[ll] = 0;
                            }

                            //
                            // Update singular vectors if desired
                            //
                            if (ncvt > 0)
                            {
                                rotations.applyrotationsfromtheleft(!fwddir, ll + vstart - 1, m + vstart - 1, vstart, vend, work2, work3, ref vt, ref vttemp);
                            }
                            if (nru > 0)
                            {
                                rotations.applyrotationsfromtheright(!fwddir, ustart, uend, ll + ustart - 1, m + ustart - 1, work0, work1, ref u, ref utemp);
                            }
                            if (ncc > 0)
                            {
                                rotations.applyrotationsfromtheleft(!fwddir, ll + cstart - 1, m + cstart - 1, cstart, cend, work0, work1, ref c, ref ctemp);
                            }
                        }
                    }

                    //
                    // QR iteration finished, go back and check convergence
                    //
                    continue;
                }

                //
                // All singular values converged, so make them positive
                //
                for (i = 1; i <= n; i++)
                {
                    if ((double)(d[i]) < (double)(0))
                    {
                        d[i] = -d[i];

                        //
                        // Change sign of singular vectors, if desired
                        //
                        if (ncvt > 0)
                        {
                            for (i_ = vstart; i_ <= vend; i_++)
                            {
                                vt[i + vstart - 1, i_] = -1 * vt[i + vstart - 1, i_];
                            }
                        }
                    }
                }

                //
                // Sort the singular values into decreasing order (insertion sort on
                // singular values, but only one transposition per singular vector)
                //
                for (i = 1; i <= n - 1; i++)
                {

                    //
                    // Scan for smallest D(I)
                    //
                    isub = 1;
                    smin = d[1];
                    for (j = 2; j <= n + 1 - i; j++)
                    {
                        if ((double)(d[j]) <= (double)(smin))
                        {
                            isub = j;
                            smin = d[j];
                        }
                    }
                    if (isub != n + 1 - i)
                    {

                        //
                        // Swap singular values and vectors
                        //
                        d[isub] = d[n + 1 - i];
                        d[n + 1 - i] = smin;
                        if (ncvt > 0)
                        {
                            j = n + 1 - i;
                            for (i_ = vstart; i_ <= vend; i_++)
                            {
                                vttemp[i_] = vt[isub + vstart - 1, i_];
                            }
                            for (i_ = vstart; i_ <= vend; i_++)
                            {
                                vt[isub + vstart - 1, i_] = vt[j + vstart - 1, i_];
                            }
                            for (i_ = vstart; i_ <= vend; i_++)
                            {
                                vt[j + vstart - 1, i_] = vttemp[i_];
                            }
                        }
                        if (nru > 0)
                        {
                            j = n + 1 - i;
                            for (i_ = ustart; i_ <= uend; i_++)
                            {
                                utemp[i_] = u[i_, isub + ustart - 1];
                            }
                            for (i_ = ustart; i_ <= uend; i_++)
                            {
                                u[i_, isub + ustart - 1] = u[i_, j + ustart - 1];
                            }
                            for (i_ = ustart; i_ <= uend; i_++)
                            {
                                u[i_, j + ustart - 1] = utemp[i_];
                            }
                        }
                        if (ncc > 0)
                        {
                            j = n + 1 - i;
                            for (i_ = cstart; i_ <= cend; i_++)
                            {
                                ctemp[i_] = c[isub + cstart - 1, i_];
                            }
                            for (i_ = cstart; i_ <= cend; i_++)
                            {
                                c[isub + cstart - 1, i_] = c[j + cstart - 1, i_];
                            }
                            for (i_ = cstart; i_ <= cend; i_++)
                            {
                                c[j + cstart - 1, i_] = ctemp[i_];
                            }
                        }
                    }
                }
                return result;
            }


            private static double extsignbdsqr(double a,
                double b)
            {
                double result = 0;

                if ((double)(b) >= (double)(0))
                {
                    result = Math.Abs(a);
                }
                else
                {
                    result = -Math.Abs(a);
                }
                return result;
            }


            private static void svd2x2(double f,
                double g,
                double h,
                ref double ssmin,
                ref double ssmax)
            {
                double aas = 0;
                double at = 0;
                double au = 0;
                double c = 0;
                double fa = 0;
                double fhmn = 0;
                double fhmx = 0;
                double ga = 0;
                double ha = 0;

                ssmin = 0;
                ssmax = 0;

                fa = Math.Abs(f);
                ga = Math.Abs(g);
                ha = Math.Abs(h);
                fhmn = Math.Min(fa, ha);
                fhmx = Math.Max(fa, ha);
                if ((double)(fhmn) == (double)(0))
                {
                    ssmin = 0;
                    if ((double)(fhmx) == (double)(0))
                    {
                        ssmax = ga;
                    }
                    else
                    {
                        ssmax = Math.Max(fhmx, ga) * Math.Sqrt(1 + math.sqr(Math.Min(fhmx, ga) / Math.Max(fhmx, ga)));
                    }
                }
                else
                {
                    if ((double)(ga) < (double)(fhmx))
                    {
                        aas = 1 + fhmn / fhmx;
                        at = (fhmx - fhmn) / fhmx;
                        au = math.sqr(ga / fhmx);
                        c = 2 / (Math.Sqrt(aas * aas + au) + Math.Sqrt(at * at + au));
                        ssmin = fhmn * c;
                        ssmax = fhmx / c;
                    }
                    else
                    {
                        au = fhmx / ga;
                        if ((double)(au) == (double)(0))
                        {

                            //
                            // Avoid possible harmful underflow if exponent range
                            // asymmetric (true SSMIN may not underflow even if
                            // AU underflows)
                            //
                            ssmin = fhmn * fhmx / ga;
                            ssmax = ga;
                        }
                        else
                        {
                            aas = 1 + fhmn / fhmx;
                            at = (fhmx - fhmn) / fhmx;
                            c = 1 / (Math.Sqrt(1 + math.sqr(aas * au)) + Math.Sqrt(1 + math.sqr(at * au)));
                            ssmin = fhmn * c * au;
                            ssmin = ssmin + ssmin;
                            ssmax = ga / (c + c);
                        }
                    }
                }
            }


            private static void svdv2x2(double f,
                double g,
                double h,
                ref double ssmin,
                ref double ssmax,
                ref double snr,
                ref double csr,
                ref double snl,
                ref double csl)
            {
                bool gasmal = new bool();
                bool swp = new bool();
                int pmax = 0;
                double a = 0;
                double clt = 0;
                double crt = 0;
                double d = 0;
                double fa = 0;
                double ft = 0;
                double ga = 0;
                double gt = 0;
                double ha = 0;
                double ht = 0;
                double l = 0;
                double m = 0;
                double mm = 0;
                double r = 0;
                double s = 0;
                double slt = 0;
                double srt = 0;
                double t = 0;
                double temp = 0;
                double tsign = 0;
                double tt = 0;
                double v = 0;

                ssmin = 0;
                ssmax = 0;
                snr = 0;
                csr = 0;
                snl = 0;
                csl = 0;

                ft = f;
                fa = Math.Abs(ft);
                ht = h;
                ha = Math.Abs(h);

                //
                // these initializers are not really necessary,
                // but without them compiler complains about uninitialized locals
                //
                clt = 0;
                crt = 0;
                slt = 0;
                srt = 0;
                tsign = 0;

                //
                // PMAX points to the maximum absolute element of matrix
                //  PMAX = 1 if F largest in absolute values
                //  PMAX = 2 if G largest in absolute values
                //  PMAX = 3 if H largest in absolute values
                //
                pmax = 1;
                swp = (double)(ha) > (double)(fa);
                if (swp)
                {

                    //
                    // Now FA .ge. HA
                    //
                    pmax = 3;
                    temp = ft;
                    ft = ht;
                    ht = temp;
                    temp = fa;
                    fa = ha;
                    ha = temp;
                }
                gt = g;
                ga = Math.Abs(gt);
                if ((double)(ga) == (double)(0))
                {

                    //
                    // Diagonal matrix
                    //
                    ssmin = ha;
                    ssmax = fa;
                    clt = 1;
                    crt = 1;
                    slt = 0;
                    srt = 0;
                }
                else
                {
                    gasmal = true;
                    if ((double)(ga) > (double)(fa))
                    {
                        pmax = 2;
                        if ((double)(fa / ga) < (double)(math.machineepsilon))
                        {

                            //
                            // Case of very large GA
                            //
                            gasmal = false;
                            ssmax = ga;
                            if ((double)(ha) > (double)(1))
                            {
                                v = ga / ha;
                                ssmin = fa / v;
                            }
                            else
                            {
                                v = fa / ga;
                                ssmin = v * ha;
                            }
                            clt = 1;
                            slt = ht / gt;
                            srt = 1;
                            crt = ft / gt;
                        }
                    }
                    if (gasmal)
                    {

                        //
                        // Normal case
                        //
                        d = fa - ha;
                        if ((double)(d) == (double)(fa))
                        {
                            l = 1;
                        }
                        else
                        {
                            l = d / fa;
                        }
                        m = gt / ft;
                        t = 2 - l;
                        mm = m * m;
                        tt = t * t;
                        s = Math.Sqrt(tt + mm);
                        if ((double)(l) == (double)(0))
                        {
                            r = Math.Abs(m);
                        }
                        else
                        {
                            r = Math.Sqrt(l * l + mm);
                        }
                        a = 0.5 * (s + r);
                        ssmin = ha / a;
                        ssmax = fa * a;
                        if ((double)(mm) == (double)(0))
                        {

                            //
                            // Note that M is very tiny
                            //
                            if ((double)(l) == (double)(0))
                            {
                                t = extsignbdsqr(2, ft) * extsignbdsqr(1, gt);
                            }
                            else
                            {
                                t = gt / extsignbdsqr(d, ft) + m / t;
                            }
                        }
                        else
                        {
                            t = (m / (s + t) + m / (r + l)) * (1 + a);
                        }
                        l = Math.Sqrt(t * t + 4);
                        crt = 2 / l;
                        srt = t / l;
                        clt = (crt + srt * m) / a;
                        v = ht / ft;
                        slt = v * srt / a;
                    }
                }
                if (swp)
                {
                    csl = srt;
                    snl = crt;
                    csr = slt;
                    snr = clt;
                }
                else
                {
                    csl = clt;
                    snl = slt;
                    csr = crt;
                    snr = srt;
                }

                //
                // Correct signs of SSMAX and SSMIN
                //
                if (pmax == 1)
                {
                    tsign = extsignbdsqr(1, csr) * extsignbdsqr(1, csl) * extsignbdsqr(1, f);
                }
                if (pmax == 2)
                {
                    tsign = extsignbdsqr(1, snr) * extsignbdsqr(1, csl) * extsignbdsqr(1, g);
                }
                if (pmax == 3)
                {
                    tsign = extsignbdsqr(1, snr) * extsignbdsqr(1, snl) * extsignbdsqr(1, h);
                }
                ssmax = extsignbdsqr(ssmax, tsign);
                ssmin = extsignbdsqr(ssmin, tsign * extsignbdsqr(1, f) * extsignbdsqr(1, h));
            }


        }
        public class ortfac
        {
            /*************************************************************************
            QR decomposition of a rectangular matrix of size MxN

            Input parameters:
                A   -   matrix A whose indexes range within [0..M-1, 0..N-1].
                M   -   number of rows in matrix A.
                N   -   number of columns in matrix A.

            Output parameters:
                A   -   matrices Q and R in compact form (see below).
                Tau -   array of scalar factors which are used to form
                        matrix Q. Array whose index ranges within [0.. Min(M-1,N-1)].

            Matrix A is represented as A = QR, where Q is an orthogonal matrix of size
            MxM, R - upper triangular (or upper trapezoid) matrix of size M x N.

            The elements of matrix R are located on and above the main diagonal of
            matrix A. The elements which are located in Tau array and below the main
            diagonal of matrix A are used to form matrix Q as follows:

            Matrix Q is represented as a product of elementary reflections

            Q = H(0)*H(2)*...*H(k-1),

            where k = min(m,n), and each H(i) is in the form

            H(i) = 1 - tau * v * (v^T)

            where tau is a scalar stored in Tau[I]; v - real vector,
            so that v(0:i-1) = 0, v(i) = 1, v(i+1:m-1) stored in A(i+1:m-1,i).

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixqr(ref double[,] a,
                int m,
                int n,
                ref double[] tau)
            {
                double[] work = new double[0];
                double[] t = new double[0];
                double[] taubuf = new double[0];
                int minmn = 0;
                double[,] tmpa = new double[0, 0];
                double[,] tmpt = new double[0, 0];
                double[,] tmpr = new double[0, 0];
                int blockstart = 0;
                int blocksize = 0;
                int rowscount = 0;
                int i = 0;
                int i_ = 0;
                int i1_ = 0;

                tau = new double[0];

                if (m <= 0 | n <= 0)
                {
                    return;
                }
                minmn = Math.Min(m, n);
                work = new double[Math.Max(m, n) + 1];
                t = new double[Math.Max(m, n) + 1];
                tau = new double[minmn];
                taubuf = new double[minmn];
                tmpa = new double[m, ablas.ablasblocksize(a)];
                tmpt = new double[ablas.ablasblocksize(a), 2 * ablas.ablasblocksize(a)];
                tmpr = new double[2 * ablas.ablasblocksize(a), n];

                //
                // Blocked code
                //
                blockstart = 0;
                while (blockstart != minmn)
                {

                    //
                    // Determine block size
                    //
                    blocksize = minmn - blockstart;
                    if (blocksize > ablas.ablasblocksize(a))
                    {
                        blocksize = ablas.ablasblocksize(a);
                    }
                    rowscount = m - blockstart;

                    //
                    // QR decomposition of submatrix.
                    // Matrix is copied to temporary storage to solve
                    // some TLB issues arising from non-contiguous memory
                    // access pattern.
                    //
                    ablas.rmatrixcopy(rowscount, blocksize, a, blockstart, blockstart, ref tmpa, 0, 0);
                    rmatrixqrbasecase(ref tmpa, rowscount, blocksize, ref work, ref t, ref taubuf);
                    ablas.rmatrixcopy(rowscount, blocksize, tmpa, 0, 0, ref a, blockstart, blockstart);
                    i1_ = (0) - (blockstart);
                    for (i_ = blockstart; i_ <= blockstart + blocksize - 1; i_++)
                    {
                        tau[i_] = taubuf[i_ + i1_];
                    }

                    //
                    // Update the rest, choose between:
                    // a) Level 2 algorithm (when the rest of the matrix is small enough)
                    // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
                    //    representation for products of Householder transformations',
                    //    by R. Schreiber and C. Van Loan.
                    //
                    if (blockstart + blocksize <= n - 1)
                    {
                        if (n - blockstart - blocksize >= 2 * ablas.ablasblocksize(a) | rowscount >= 4 * ablas.ablasblocksize(a))
                        {

                            //
                            // Prepare block reflector
                            //
                            rmatrixblockreflector(ref tmpa, ref taubuf, true, rowscount, blocksize, ref tmpt, ref work);

                            //
                            // Multiply the rest of A by Q'.
                            //
                            // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
                            // Q' = E + Y*T'*Y' = E + TmpA*TmpT'*TmpA'
                            //
                            ablas.rmatrixgemm(blocksize, n - blockstart - blocksize, rowscount, 1.0, tmpa, 0, 0, 1, a, blockstart, blockstart + blocksize, 0, 0.0, ref tmpr, 0, 0);
                            ablas.rmatrixgemm(blocksize, n - blockstart - blocksize, blocksize, 1.0, tmpt, 0, 0, 1, tmpr, 0, 0, 0, 0.0, ref tmpr, blocksize, 0);
                            ablas.rmatrixgemm(rowscount, n - blockstart - blocksize, blocksize, 1.0, tmpa, 0, 0, 0, tmpr, blocksize, 0, 0, 1.0, ref a, blockstart, blockstart + blocksize);
                        }
                        else
                        {

                            //
                            // Level 2 algorithm
                            //
                            for (i = 0; i <= blocksize - 1; i++)
                            {
                                i1_ = (i) - (1);
                                for (i_ = 1; i_ <= rowscount - i; i_++)
                                {
                                    t[i_] = tmpa[i_ + i1_, i];
                                }
                                t[1] = 1;
                                reflections.applyreflectionfromtheleft(ref a, taubuf[i], t, blockstart + i, m - 1, blockstart + blocksize, n - 1, ref work);
                            }
                        }
                    }

                    //
                    // Advance
                    //
                    blockstart = blockstart + blocksize;
                }
            }


            /*************************************************************************
            LQ decomposition of a rectangular matrix of size MxN

            Input parameters:
                A   -   matrix A whose indexes range within [0..M-1, 0..N-1].
                M   -   number of rows in matrix A.
                N   -   number of columns in matrix A.

            Output parameters:
                A   -   matrices L and Q in compact form (see below)
                Tau -   array of scalar factors which are used to form
                        matrix Q. Array whose index ranges within [0..Min(M,N)-1].

            Matrix A is represented as A = LQ, where Q is an orthogonal matrix of size
            MxM, L - lower triangular (or lower trapezoid) matrix of size M x N.

            The elements of matrix L are located on and below  the  main  diagonal  of
            matrix A. The elements which are located in Tau array and above  the  main
            diagonal of matrix A are used to form matrix Q as follows:

            Matrix Q is represented as a product of elementary reflections

            Q = H(k-1)*H(k-2)*...*H(1)*H(0),

            where k = min(m,n), and each H(i) is of the form

            H(i) = 1 - tau * v * (v^T)

            where tau is a scalar stored in Tau[I]; v - real vector, so that v(0:i-1)=0,
            v(i) = 1, v(i+1:n-1) stored in A(i,i+1:n-1).

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixlq(ref double[,] a,
                int m,
                int n,
                ref double[] tau)
            {
                double[] work = new double[0];
                double[] t = new double[0];
                double[] taubuf = new double[0];
                int minmn = 0;
                double[,] tmpa = new double[0, 0];
                double[,] tmpt = new double[0, 0];
                double[,] tmpr = new double[0, 0];
                int blockstart = 0;
                int blocksize = 0;
                int columnscount = 0;
                int i = 0;
                int i_ = 0;
                int i1_ = 0;

                tau = new double[0];

                if (m <= 0 | n <= 0)
                {
                    return;
                }
                minmn = Math.Min(m, n);
                work = new double[Math.Max(m, n) + 1];
                t = new double[Math.Max(m, n) + 1];
                tau = new double[minmn];
                taubuf = new double[minmn];
                tmpa = new double[ablas.ablasblocksize(a), n];
                tmpt = new double[ablas.ablasblocksize(a), 2 * ablas.ablasblocksize(a)];
                tmpr = new double[m, 2 * ablas.ablasblocksize(a)];

                //
                // Blocked code
                //
                blockstart = 0;
                while (blockstart != minmn)
                {

                    //
                    // Determine block size
                    //
                    blocksize = minmn - blockstart;
                    if (blocksize > ablas.ablasblocksize(a))
                    {
                        blocksize = ablas.ablasblocksize(a);
                    }
                    columnscount = n - blockstart;

                    //
                    // LQ decomposition of submatrix.
                    // Matrix is copied to temporary storage to solve
                    // some TLB issues arising from non-contiguous memory
                    // access pattern.
                    //
                    ablas.rmatrixcopy(blocksize, columnscount, a, blockstart, blockstart, ref tmpa, 0, 0);
                    rmatrixlqbasecase(ref tmpa, blocksize, columnscount, ref work, ref t, ref taubuf);
                    ablas.rmatrixcopy(blocksize, columnscount, tmpa, 0, 0, ref a, blockstart, blockstart);
                    i1_ = (0) - (blockstart);
                    for (i_ = blockstart; i_ <= blockstart + blocksize - 1; i_++)
                    {
                        tau[i_] = taubuf[i_ + i1_];
                    }

                    //
                    // Update the rest, choose between:
                    // a) Level 2 algorithm (when the rest of the matrix is small enough)
                    // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
                    //    representation for products of Householder transformations',
                    //    by R. Schreiber and C. Van Loan.
                    //
                    if (blockstart + blocksize <= m - 1)
                    {
                        if (m - blockstart - blocksize >= 2 * ablas.ablasblocksize(a))
                        {

                            //
                            // Prepare block reflector
                            //
                            rmatrixblockreflector(ref tmpa, ref taubuf, false, columnscount, blocksize, ref tmpt, ref work);

                            //
                            // Multiply the rest of A by Q.
                            //
                            // Q  = E + Y*T*Y'  = E + TmpA'*TmpT*TmpA
                            //
                            ablas.rmatrixgemm(m - blockstart - blocksize, blocksize, columnscount, 1.0, a, blockstart + blocksize, blockstart, 0, tmpa, 0, 0, 1, 0.0, ref tmpr, 0, 0);
                            ablas.rmatrixgemm(m - blockstart - blocksize, blocksize, blocksize, 1.0, tmpr, 0, 0, 0, tmpt, 0, 0, 0, 0.0, ref tmpr, 0, blocksize);
                            ablas.rmatrixgemm(m - blockstart - blocksize, columnscount, blocksize, 1.0, tmpr, 0, blocksize, 0, tmpa, 0, 0, 0, 1.0, ref a, blockstart + blocksize, blockstart);
                        }
                        else
                        {

                            //
                            // Level 2 algorithm
                            //
                            for (i = 0; i <= blocksize - 1; i++)
                            {
                                i1_ = (i) - (1);
                                for (i_ = 1; i_ <= columnscount - i; i_++)
                                {
                                    t[i_] = tmpa[i, i_ + i1_];
                                }
                                t[1] = 1;
                                reflections.applyreflectionfromtheright(ref a, taubuf[i], t, blockstart + blocksize, m - 1, blockstart + i, n - 1, ref work);
                            }
                        }
                    }

                    //
                    // Advance
                    //
                    blockstart = blockstart + blocksize;
                }
            }


            /*************************************************************************
            QR decomposition of a rectangular complex matrix of size MxN

            Input parameters:
                A   -   matrix A whose indexes range within [0..M-1, 0..N-1]
                M   -   number of rows in matrix A.
                N   -   number of columns in matrix A.

            Output parameters:
                A   -   matrices Q and R in compact form
                Tau -   array of scalar factors which are used to form matrix Q. Array
                        whose indexes range within [0.. Min(M,N)-1]

            Matrix A is represented as A = QR, where Q is an orthogonal matrix of size
            MxM, R - upper triangular (or upper trapezoid) matrix of size MxN.

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994
            *************************************************************************/
            public static void cmatrixqr(ref complex[,] a,
                int m,
                int n,
                ref complex[] tau)
            {
                complex[] work = new complex[0];
                complex[] t = new complex[0];
                complex[] taubuf = new complex[0];
                int minmn = 0;
                complex[,] tmpa = new complex[0, 0];
                complex[,] tmpt = new complex[0, 0];
                complex[,] tmpr = new complex[0, 0];
                int blockstart = 0;
                int blocksize = 0;
                int rowscount = 0;
                int i = 0;
                int i_ = 0;
                int i1_ = 0;

                tau = new complex[0];

                if (m <= 0 | n <= 0)
                {
                    return;
                }
                minmn = Math.Min(m, n);
                work = new complex[Math.Max(m, n) + 1];
                t = new complex[Math.Max(m, n) + 1];
                tau = new complex[minmn];
                taubuf = new complex[minmn];
                tmpa = new complex[m, ablas.ablascomplexblocksize(a)];
                tmpt = new complex[ablas.ablascomplexblocksize(a), ablas.ablascomplexblocksize(a)];
                tmpr = new complex[2 * ablas.ablascomplexblocksize(a), n];

                //
                // Blocked code
                //
                blockstart = 0;
                while (blockstart != minmn)
                {

                    //
                    // Determine block size
                    //
                    blocksize = minmn - blockstart;
                    if (blocksize > ablas.ablascomplexblocksize(a))
                    {
                        blocksize = ablas.ablascomplexblocksize(a);
                    }
                    rowscount = m - blockstart;

                    //
                    // QR decomposition of submatrix.
                    // Matrix is copied to temporary storage to solve
                    // some TLB issues arising from non-contiguous memory
                    // access pattern.
                    //
                    ablas.cmatrixcopy(rowscount, blocksize, a, blockstart, blockstart, ref tmpa, 0, 0);
                    cmatrixqrbasecase(ref tmpa, rowscount, blocksize, ref work, ref t, ref taubuf);
                    ablas.cmatrixcopy(rowscount, blocksize, tmpa, 0, 0, ref a, blockstart, blockstart);
                    i1_ = (0) - (blockstart);
                    for (i_ = blockstart; i_ <= blockstart + blocksize - 1; i_++)
                    {
                        tau[i_] = taubuf[i_ + i1_];
                    }

                    //
                    // Update the rest, choose between:
                    // a) Level 2 algorithm (when the rest of the matrix is small enough)
                    // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
                    //    representation for products of Householder transformations',
                    //    by R. Schreiber and C. Van Loan.
                    //
                    if (blockstart + blocksize <= n - 1)
                    {
                        if (n - blockstart - blocksize >= 2 * ablas.ablascomplexblocksize(a))
                        {

                            //
                            // Prepare block reflector
                            //
                            cmatrixblockreflector(ref tmpa, ref taubuf, true, rowscount, blocksize, ref tmpt, ref work);

                            //
                            // Multiply the rest of A by Q'.
                            //
                            // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
                            // Q' = E + Y*T'*Y' = E + TmpA*TmpT'*TmpA'
                            //
                            ablas.cmatrixgemm(blocksize, n - blockstart - blocksize, rowscount, 1.0, tmpa, 0, 0, 2, a, blockstart, blockstart + blocksize, 0, 0.0, ref tmpr, 0, 0);
                            ablas.cmatrixgemm(blocksize, n - blockstart - blocksize, blocksize, 1.0, tmpt, 0, 0, 2, tmpr, 0, 0, 0, 0.0, ref tmpr, blocksize, 0);
                            ablas.cmatrixgemm(rowscount, n - blockstart - blocksize, blocksize, 1.0, tmpa, 0, 0, 0, tmpr, blocksize, 0, 0, 1.0, ref a, blockstart, blockstart + blocksize);
                        }
                        else
                        {

                            //
                            // Level 2 algorithm
                            //
                            for (i = 0; i <= blocksize - 1; i++)
                            {
                                i1_ = (i) - (1);
                                for (i_ = 1; i_ <= rowscount - i; i_++)
                                {
                                    t[i_] = tmpa[i_ + i1_, i];
                                }
                                t[1] = 1;
                                creflections.complexapplyreflectionfromtheleft(ref a, math.conj(taubuf[i]), t, blockstart + i, m - 1, blockstart + blocksize, n - 1, ref work);
                            }
                        }
                    }

                    //
                    // Advance
                    //
                    blockstart = blockstart + blocksize;
                }
            }


            /*************************************************************************
            LQ decomposition of a rectangular complex matrix of size MxN

            Input parameters:
                A   -   matrix A whose indexes range within [0..M-1, 0..N-1]
                M   -   number of rows in matrix A.
                N   -   number of columns in matrix A.

            Output parameters:
                A   -   matrices Q and L in compact form
                Tau -   array of scalar factors which are used to form matrix Q. Array
                        whose indexes range within [0.. Min(M,N)-1]

            Matrix A is represented as A = LQ, where Q is an orthogonal matrix of size
            MxM, L - lower triangular (or lower trapezoid) matrix of size MxN.

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994
            *************************************************************************/
            public static void cmatrixlq(ref complex[,] a,
                int m,
                int n,
                ref complex[] tau)
            {
                complex[] work = new complex[0];
                complex[] t = new complex[0];
                complex[] taubuf = new complex[0];
                int minmn = 0;
                complex[,] tmpa = new complex[0, 0];
                complex[,] tmpt = new complex[0, 0];
                complex[,] tmpr = new complex[0, 0];
                int blockstart = 0;
                int blocksize = 0;
                int columnscount = 0;
                int i = 0;
                int i_ = 0;
                int i1_ = 0;

                tau = new complex[0];

                if (m <= 0 | n <= 0)
                {
                    return;
                }
                minmn = Math.Min(m, n);
                work = new complex[Math.Max(m, n) + 1];
                t = new complex[Math.Max(m, n) + 1];
                tau = new complex[minmn];
                taubuf = new complex[minmn];
                tmpa = new complex[ablas.ablascomplexblocksize(a), n];
                tmpt = new complex[ablas.ablascomplexblocksize(a), ablas.ablascomplexblocksize(a)];
                tmpr = new complex[m, 2 * ablas.ablascomplexblocksize(a)];

                //
                // Blocked code
                //
                blockstart = 0;
                while (blockstart != minmn)
                {

                    //
                    // Determine block size
                    //
                    blocksize = minmn - blockstart;
                    if (blocksize > ablas.ablascomplexblocksize(a))
                    {
                        blocksize = ablas.ablascomplexblocksize(a);
                    }
                    columnscount = n - blockstart;

                    //
                    // LQ decomposition of submatrix.
                    // Matrix is copied to temporary storage to solve
                    // some TLB issues arising from non-contiguous memory
                    // access pattern.
                    //
                    ablas.cmatrixcopy(blocksize, columnscount, a, blockstart, blockstart, ref tmpa, 0, 0);
                    cmatrixlqbasecase(ref tmpa, blocksize, columnscount, ref work, ref t, ref taubuf);
                    ablas.cmatrixcopy(blocksize, columnscount, tmpa, 0, 0, ref a, blockstart, blockstart);
                    i1_ = (0) - (blockstart);
                    for (i_ = blockstart; i_ <= blockstart + blocksize - 1; i_++)
                    {
                        tau[i_] = taubuf[i_ + i1_];
                    }

                    //
                    // Update the rest, choose between:
                    // a) Level 2 algorithm (when the rest of the matrix is small enough)
                    // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
                    //    representation for products of Householder transformations',
                    //    by R. Schreiber and C. Van Loan.
                    //
                    if (blockstart + blocksize <= m - 1)
                    {
                        if (m - blockstart - blocksize >= 2 * ablas.ablascomplexblocksize(a))
                        {

                            //
                            // Prepare block reflector
                            //
                            cmatrixblockreflector(ref tmpa, ref taubuf, false, columnscount, blocksize, ref tmpt, ref work);

                            //
                            // Multiply the rest of A by Q.
                            //
                            // Q  = E + Y*T*Y'  = E + TmpA'*TmpT*TmpA
                            //
                            ablas.cmatrixgemm(m - blockstart - blocksize, blocksize, columnscount, 1.0, a, blockstart + blocksize, blockstart, 0, tmpa, 0, 0, 2, 0.0, ref tmpr, 0, 0);
                            ablas.cmatrixgemm(m - blockstart - blocksize, blocksize, blocksize, 1.0, tmpr, 0, 0, 0, tmpt, 0, 0, 0, 0.0, ref tmpr, 0, blocksize);
                            ablas.cmatrixgemm(m - blockstart - blocksize, columnscount, blocksize, 1.0, tmpr, 0, blocksize, 0, tmpa, 0, 0, 0, 1.0, ref a, blockstart + blocksize, blockstart);
                        }
                        else
                        {

                            //
                            // Level 2 algorithm
                            //
                            for (i = 0; i <= blocksize - 1; i++)
                            {
                                i1_ = (i) - (1);
                                for (i_ = 1; i_ <= columnscount - i; i_++)
                                {
                                    t[i_] = math.conj(tmpa[i, i_ + i1_]);
                                }
                                t[1] = 1;
                                creflections.complexapplyreflectionfromtheright(ref a, taubuf[i], ref t, blockstart + blocksize, m - 1, blockstart + i, n - 1, ref work);
                            }
                        }
                    }

                    //
                    // Advance
                    //
                    blockstart = blockstart + blocksize;
                }
            }


            /*************************************************************************
            Partial unpacking of matrix Q from the QR decomposition of a matrix A

            Input parameters:
                A       -   matrices Q and R in compact form.
                            Output of RMatrixQR subroutine.
                M       -   number of rows in given matrix A. M>=0.
                N       -   number of columns in given matrix A. N>=0.
                Tau     -   scalar factors which are used to form Q.
                            Output of the RMatrixQR subroutine.
                QColumns -  required number of columns of matrix Q. M>=QColumns>=0.

            Output parameters:
                Q       -   first QColumns columns of matrix Q.
                            Array whose indexes range within [0..M-1, 0..QColumns-1].
                            If QColumns=0, the array remains unchanged.

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixqrunpackq(double[,] a,
                int m,
                int n,
                double[] tau,
                int qcolumns,
                ref double[,] q)
            {
                double[] work = new double[0];
                double[] t = new double[0];
                double[] taubuf = new double[0];
                int minmn = 0;
                int refcnt = 0;
                double[,] tmpa = new double[0, 0];
                double[,] tmpt = new double[0, 0];
                double[,] tmpr = new double[0, 0];
                int blockstart = 0;
                int blocksize = 0;
                int rowscount = 0;
                int i = 0;
                int j = 0;
                int i_ = 0;
                int i1_ = 0;

                q = new double[0, 0];

                ap.assert(qcolumns <= m, "UnpackQFromQR: QColumns>M!");
                if ((m <= 0 | n <= 0) | qcolumns <= 0)
                {
                    return;
                }

                //
                // init
                //
                minmn = Math.Min(m, n);
                refcnt = Math.Min(minmn, qcolumns);
                q = new double[m, qcolumns];
                for (i = 0; i <= m - 1; i++)
                {
                    for (j = 0; j <= qcolumns - 1; j++)
                    {
                        if (i == j)
                        {
                            q[i, j] = 1;
                        }
                        else
                        {
                            q[i, j] = 0;
                        }
                    }
                }
                work = new double[Math.Max(m, qcolumns) + 1];
                t = new double[Math.Max(m, qcolumns) + 1];
                taubuf = new double[minmn];
                tmpa = new double[m, ablas.ablasblocksize(a)];
                tmpt = new double[ablas.ablasblocksize(a), 2 * ablas.ablasblocksize(a)];
                tmpr = new double[2 * ablas.ablasblocksize(a), qcolumns];

                //
                // Blocked code
                //
                blockstart = ablas.ablasblocksize(a) * (refcnt / ablas.ablasblocksize(a));
                blocksize = refcnt - blockstart;
                while (blockstart >= 0)
                {
                    rowscount = m - blockstart;

                    //
                    // Copy current block
                    //
                    ablas.rmatrixcopy(rowscount, blocksize, a, blockstart, blockstart, ref tmpa, 0, 0);
                    i1_ = (blockstart) - (0);
                    for (i_ = 0; i_ <= blocksize - 1; i_++)
                    {
                        taubuf[i_] = tau[i_ + i1_];
                    }

                    //
                    // Update, choose between:
                    // a) Level 2 algorithm (when the rest of the matrix is small enough)
                    // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
                    //    representation for products of Householder transformations',
                    //    by R. Schreiber and C. Van Loan.
                    //
                    if (qcolumns >= 2 * ablas.ablasblocksize(a))
                    {

                        //
                        // Prepare block reflector
                        //
                        rmatrixblockreflector(ref tmpa, ref taubuf, true, rowscount, blocksize, ref tmpt, ref work);

                        //
                        // Multiply matrix by Q.
                        //
                        // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
                        //
                        ablas.rmatrixgemm(blocksize, qcolumns, rowscount, 1.0, tmpa, 0, 0, 1, q, blockstart, 0, 0, 0.0, ref tmpr, 0, 0);
                        ablas.rmatrixgemm(blocksize, qcolumns, blocksize, 1.0, tmpt, 0, 0, 0, tmpr, 0, 0, 0, 0.0, ref tmpr, blocksize, 0);
                        ablas.rmatrixgemm(rowscount, qcolumns, blocksize, 1.0, tmpa, 0, 0, 0, tmpr, blocksize, 0, 0, 1.0, ref q, blockstart, 0);
                    }
                    else
                    {

                        //
                        // Level 2 algorithm
                        //
                        for (i = blocksize - 1; i >= 0; i--)
                        {
                            i1_ = (i) - (1);
                            for (i_ = 1; i_ <= rowscount - i; i_++)
                            {
                                t[i_] = tmpa[i_ + i1_, i];
                            }
                            t[1] = 1;
                            reflections.applyreflectionfromtheleft(ref q, taubuf[i], t, blockstart + i, m - 1, 0, qcolumns - 1, ref work);
                        }
                    }

                    //
                    // Advance
                    //
                    blockstart = blockstart - ablas.ablasblocksize(a);
                    blocksize = ablas.ablasblocksize(a);
                }
            }


            /*************************************************************************
            Unpacking of matrix R from the QR decomposition of a matrix A

            Input parameters:
                A       -   matrices Q and R in compact form.
                            Output of RMatrixQR subroutine.
                M       -   number of rows in given matrix A. M>=0.
                N       -   number of columns in given matrix A. N>=0.

            Output parameters:
                R       -   matrix R, array[0..M-1, 0..N-1].

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixqrunpackr(double[,] a,
                int m,
                int n,
                ref double[,] r)
            {
                int i = 0;
                int k = 0;
                int i_ = 0;

                r = new double[0, 0];

                if (m <= 0 | n <= 0)
                {
                    return;
                }
                k = Math.Min(m, n);
                r = new double[m, n];
                for (i = 0; i <= n - 1; i++)
                {
                    r[0, i] = 0;
                }
                for (i = 1; i <= m - 1; i++)
                {
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        r[i, i_] = r[0, i_];
                    }
                }
                for (i = 0; i <= k - 1; i++)
                {
                    for (i_ = i; i_ <= n - 1; i_++)
                    {
                        r[i, i_] = a[i, i_];
                    }
                }
            }


            /*************************************************************************
            Partial unpacking of matrix Q from the LQ decomposition of a matrix A

            Input parameters:
                A       -   matrices L and Q in compact form.
                            Output of RMatrixLQ subroutine.
                M       -   number of rows in given matrix A. M>=0.
                N       -   number of columns in given matrix A. N>=0.
                Tau     -   scalar factors which are used to form Q.
                            Output of the RMatrixLQ subroutine.
                QRows   -   required number of rows in matrix Q. N>=QRows>=0.

            Output parameters:
                Q       -   first QRows rows of matrix Q. Array whose indexes range
                            within [0..QRows-1, 0..N-1]. If QRows=0, the array remains
                            unchanged.

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixlqunpackq(double[,] a,
                int m,
                int n,
                double[] tau,
                int qrows,
                ref double[,] q)
            {
                double[] work = new double[0];
                double[] t = new double[0];
                double[] taubuf = new double[0];
                int minmn = 0;
                int refcnt = 0;
                double[,] tmpa = new double[0, 0];
                double[,] tmpt = new double[0, 0];
                double[,] tmpr = new double[0, 0];
                int blockstart = 0;
                int blocksize = 0;
                int columnscount = 0;
                int i = 0;
                int j = 0;
                int i_ = 0;
                int i1_ = 0;

                q = new double[0, 0];

                ap.assert(qrows <= n, "RMatrixLQUnpackQ: QRows>N!");
                if ((m <= 0 | n <= 0) | qrows <= 0)
                {
                    return;
                }

                //
                // init
                //
                minmn = Math.Min(m, n);
                refcnt = Math.Min(minmn, qrows);
                work = new double[Math.Max(m, n) + 1];
                t = new double[Math.Max(m, n) + 1];
                taubuf = new double[minmn];
                tmpa = new double[ablas.ablasblocksize(a), n];
                tmpt = new double[ablas.ablasblocksize(a), 2 * ablas.ablasblocksize(a)];
                tmpr = new double[qrows, 2 * ablas.ablasblocksize(a)];
                q = new double[qrows, n];
                for (i = 0; i <= qrows - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (i == j)
                        {
                            q[i, j] = 1;
                        }
                        else
                        {
                            q[i, j] = 0;
                        }
                    }
                }

                //
                // Blocked code
                //
                blockstart = ablas.ablasblocksize(a) * (refcnt / ablas.ablasblocksize(a));
                blocksize = refcnt - blockstart;
                while (blockstart >= 0)
                {
                    columnscount = n - blockstart;

                    //
                    // Copy submatrix
                    //
                    ablas.rmatrixcopy(blocksize, columnscount, a, blockstart, blockstart, ref tmpa, 0, 0);
                    i1_ = (blockstart) - (0);
                    for (i_ = 0; i_ <= blocksize - 1; i_++)
                    {
                        taubuf[i_] = tau[i_ + i1_];
                    }

                    //
                    // Update matrix, choose between:
                    // a) Level 2 algorithm (when the rest of the matrix is small enough)
                    // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
                    //    representation for products of Householder transformations',
                    //    by R. Schreiber and C. Van Loan.
                    //
                    if (qrows >= 2 * ablas.ablasblocksize(a))
                    {

                        //
                        // Prepare block reflector
                        //
                        rmatrixblockreflector(ref tmpa, ref taubuf, false, columnscount, blocksize, ref tmpt, ref work);

                        //
                        // Multiply the rest of A by Q'.
                        //
                        // Q'  = E + Y*T'*Y'  = E + TmpA'*TmpT'*TmpA
                        //
                        ablas.rmatrixgemm(qrows, blocksize, columnscount, 1.0, q, 0, blockstart, 0, tmpa, 0, 0, 1, 0.0, ref tmpr, 0, 0);
                        ablas.rmatrixgemm(qrows, blocksize, blocksize, 1.0, tmpr, 0, 0, 0, tmpt, 0, 0, 1, 0.0, ref tmpr, 0, blocksize);
                        ablas.rmatrixgemm(qrows, columnscount, blocksize, 1.0, tmpr, 0, blocksize, 0, tmpa, 0, 0, 0, 1.0, ref q, 0, blockstart);
                    }
                    else
                    {

                        //
                        // Level 2 algorithm
                        //
                        for (i = blocksize - 1; i >= 0; i--)
                        {
                            i1_ = (i) - (1);
                            for (i_ = 1; i_ <= columnscount - i; i_++)
                            {
                                t[i_] = tmpa[i, i_ + i1_];
                            }
                            t[1] = 1;
                            reflections.applyreflectionfromtheright(ref q, taubuf[i], t, 0, qrows - 1, blockstart + i, n - 1, ref work);
                        }
                    }

                    //
                    // Advance
                    //
                    blockstart = blockstart - ablas.ablasblocksize(a);
                    blocksize = ablas.ablasblocksize(a);
                }
            }


            /*************************************************************************
            Unpacking of matrix L from the LQ decomposition of a matrix A

            Input parameters:
                A       -   matrices Q and L in compact form.
                            Output of RMatrixLQ subroutine.
                M       -   number of rows in given matrix A. M>=0.
                N       -   number of columns in given matrix A. N>=0.

            Output parameters:
                L       -   matrix L, array[0..M-1, 0..N-1].

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixlqunpackl(double[,] a,
                int m,
                int n,
                ref double[,] l)
            {
                int i = 0;
                int k = 0;
                int i_ = 0;

                l = new double[0, 0];

                if (m <= 0 | n <= 0)
                {
                    return;
                }
                l = new double[m, n];
                for (i = 0; i <= n - 1; i++)
                {
                    l[0, i] = 0;
                }
                for (i = 1; i <= m - 1; i++)
                {
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        l[i, i_] = l[0, i_];
                    }
                }
                for (i = 0; i <= m - 1; i++)
                {
                    k = Math.Min(i, n - 1);
                    for (i_ = 0; i_ <= k; i_++)
                    {
                        l[i, i_] = a[i, i_];
                    }
                }
            }


            /*************************************************************************
            Partial unpacking of matrix Q from QR decomposition of a complex matrix A.

            Input parameters:
                A           -   matrices Q and R in compact form.
                                Output of CMatrixQR subroutine .
                M           -   number of rows in matrix A. M>=0.
                N           -   number of columns in matrix A. N>=0.
                Tau         -   scalar factors which are used to form Q.
                                Output of CMatrixQR subroutine .
                QColumns    -   required number of columns in matrix Q. M>=QColumns>=0.

            Output parameters:
                Q           -   first QColumns columns of matrix Q.
                                Array whose index ranges within [0..M-1, 0..QColumns-1].
                                If QColumns=0, array isn't changed.

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void cmatrixqrunpackq(complex[,] a,
                int m,
                int n,
                complex[] tau,
                int qcolumns,
                ref complex[,] q)
            {
                complex[] work = new complex[0];
                complex[] t = new complex[0];
                complex[] taubuf = new complex[0];
                int minmn = 0;
                int refcnt = 0;
                complex[,] tmpa = new complex[0, 0];
                complex[,] tmpt = new complex[0, 0];
                complex[,] tmpr = new complex[0, 0];
                int blockstart = 0;
                int blocksize = 0;
                int rowscount = 0;
                int i = 0;
                int j = 0;
                int i_ = 0;
                int i1_ = 0;

                q = new complex[0, 0];

                ap.assert(qcolumns <= m, "UnpackQFromQR: QColumns>M!");
                if (m <= 0 | n <= 0)
                {
                    return;
                }

                //
                // init
                //
                minmn = Math.Min(m, n);
                refcnt = Math.Min(minmn, qcolumns);
                work = new complex[Math.Max(m, n) + 1];
                t = new complex[Math.Max(m, n) + 1];
                taubuf = new complex[minmn];
                tmpa = new complex[m, ablas.ablascomplexblocksize(a)];
                tmpt = new complex[ablas.ablascomplexblocksize(a), ablas.ablascomplexblocksize(a)];
                tmpr = new complex[2 * ablas.ablascomplexblocksize(a), qcolumns];
                q = new complex[m, qcolumns];
                for (i = 0; i <= m - 1; i++)
                {
                    for (j = 0; j <= qcolumns - 1; j++)
                    {
                        if (i == j)
                        {
                            q[i, j] = 1;
                        }
                        else
                        {
                            q[i, j] = 0;
                        }
                    }
                }

                //
                // Blocked code
                //
                blockstart = ablas.ablascomplexblocksize(a) * (refcnt / ablas.ablascomplexblocksize(a));
                blocksize = refcnt - blockstart;
                while (blockstart >= 0)
                {
                    rowscount = m - blockstart;

                    //
                    // QR decomposition of submatrix.
                    // Matrix is copied to temporary storage to solve
                    // some TLB issues arising from non-contiguous memory
                    // access pattern.
                    //
                    ablas.cmatrixcopy(rowscount, blocksize, a, blockstart, blockstart, ref tmpa, 0, 0);
                    i1_ = (blockstart) - (0);
                    for (i_ = 0; i_ <= blocksize - 1; i_++)
                    {
                        taubuf[i_] = tau[i_ + i1_];
                    }

                    //
                    // Update matrix, choose between:
                    // a) Level 2 algorithm (when the rest of the matrix is small enough)
                    // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
                    //    representation for products of Householder transformations',
                    //    by R. Schreiber and C. Van Loan.
                    //
                    if (qcolumns >= 2 * ablas.ablascomplexblocksize(a))
                    {

                        //
                        // Prepare block reflector
                        //
                        cmatrixblockreflector(ref tmpa, ref taubuf, true, rowscount, blocksize, ref tmpt, ref work);

                        //
                        // Multiply the rest of A by Q.
                        //
                        // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
                        //
                        ablas.cmatrixgemm(blocksize, qcolumns, rowscount, 1.0, tmpa, 0, 0, 2, q, blockstart, 0, 0, 0.0, ref tmpr, 0, 0);
                        ablas.cmatrixgemm(blocksize, qcolumns, blocksize, 1.0, tmpt, 0, 0, 0, tmpr, 0, 0, 0, 0.0, ref tmpr, blocksize, 0);
                        ablas.cmatrixgemm(rowscount, qcolumns, blocksize, 1.0, tmpa, 0, 0, 0, tmpr, blocksize, 0, 0, 1.0, ref q, blockstart, 0);
                    }
                    else
                    {

                        //
                        // Level 2 algorithm
                        //
                        for (i = blocksize - 1; i >= 0; i--)
                        {
                            i1_ = (i) - (1);
                            for (i_ = 1; i_ <= rowscount - i; i_++)
                            {
                                t[i_] = tmpa[i_ + i1_, i];
                            }
                            t[1] = 1;
                            creflections.complexapplyreflectionfromtheleft(ref q, taubuf[i], t, blockstart + i, m - 1, 0, qcolumns - 1, ref work);
                        }
                    }

                    //
                    // Advance
                    //
                    blockstart = blockstart - ablas.ablascomplexblocksize(a);
                    blocksize = ablas.ablascomplexblocksize(a);
                }
            }


            /*************************************************************************
            Unpacking of matrix R from the QR decomposition of a matrix A

            Input parameters:
                A       -   matrices Q and R in compact form.
                            Output of CMatrixQR subroutine.
                M       -   number of rows in given matrix A. M>=0.
                N       -   number of columns in given matrix A. N>=0.

            Output parameters:
                R       -   matrix R, array[0..M-1, 0..N-1].

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void cmatrixqrunpackr(complex[,] a,
                int m,
                int n,
                ref complex[,] r)
            {
                int i = 0;
                int k = 0;
                int i_ = 0;

                r = new complex[0, 0];

                if (m <= 0 | n <= 0)
                {
                    return;
                }
                k = Math.Min(m, n);
                r = new complex[m, n];
                for (i = 0; i <= n - 1; i++)
                {
                    r[0, i] = 0;
                }
                for (i = 1; i <= m - 1; i++)
                {
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        r[i, i_] = r[0, i_];
                    }
                }
                for (i = 0; i <= k - 1; i++)
                {
                    for (i_ = i; i_ <= n - 1; i_++)
                    {
                        r[i, i_] = a[i, i_];
                    }
                }
            }


            /*************************************************************************
            Partial unpacking of matrix Q from LQ decomposition of a complex matrix A.

            Input parameters:
                A           -   matrices Q and R in compact form.
                                Output of CMatrixLQ subroutine .
                M           -   number of rows in matrix A. M>=0.
                N           -   number of columns in matrix A. N>=0.
                Tau         -   scalar factors which are used to form Q.
                                Output of CMatrixLQ subroutine .
                QRows       -   required number of rows in matrix Q. N>=QColumns>=0.

            Output parameters:
                Q           -   first QRows rows of matrix Q.
                                Array whose index ranges within [0..QRows-1, 0..N-1].
                                If QRows=0, array isn't changed.

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void cmatrixlqunpackq(complex[,] a,
                int m,
                int n,
                complex[] tau,
                int qrows,
                ref complex[,] q)
            {
                complex[] work = new complex[0];
                complex[] t = new complex[0];
                complex[] taubuf = new complex[0];
                int minmn = 0;
                int refcnt = 0;
                complex[,] tmpa = new complex[0, 0];
                complex[,] tmpt = new complex[0, 0];
                complex[,] tmpr = new complex[0, 0];
                int blockstart = 0;
                int blocksize = 0;
                int columnscount = 0;
                int i = 0;
                int j = 0;
                int i_ = 0;
                int i1_ = 0;

                q = new complex[0, 0];

                if (m <= 0 | n <= 0)
                {
                    return;
                }

                //
                // Init
                //
                minmn = Math.Min(m, n);
                refcnt = Math.Min(minmn, qrows);
                work = new complex[Math.Max(m, n) + 1];
                t = new complex[Math.Max(m, n) + 1];
                taubuf = new complex[minmn];
                tmpa = new complex[ablas.ablascomplexblocksize(a), n];
                tmpt = new complex[ablas.ablascomplexblocksize(a), ablas.ablascomplexblocksize(a)];
                tmpr = new complex[qrows, 2 * ablas.ablascomplexblocksize(a)];
                q = new complex[qrows, n];
                for (i = 0; i <= qrows - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (i == j)
                        {
                            q[i, j] = 1;
                        }
                        else
                        {
                            q[i, j] = 0;
                        }
                    }
                }

                //
                // Blocked code
                //
                blockstart = ablas.ablascomplexblocksize(a) * (refcnt / ablas.ablascomplexblocksize(a));
                blocksize = refcnt - blockstart;
                while (blockstart >= 0)
                {
                    columnscount = n - blockstart;

                    //
                    // LQ decomposition of submatrix.
                    // Matrix is copied to temporary storage to solve
                    // some TLB issues arising from non-contiguous memory
                    // access pattern.
                    //
                    ablas.cmatrixcopy(blocksize, columnscount, a, blockstart, blockstart, ref tmpa, 0, 0);
                    i1_ = (blockstart) - (0);
                    for (i_ = 0; i_ <= blocksize - 1; i_++)
                    {
                        taubuf[i_] = tau[i_ + i1_];
                    }

                    //
                    // Update matrix, choose between:
                    // a) Level 2 algorithm (when the rest of the matrix is small enough)
                    // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
                    //    representation for products of Householder transformations',
                    //    by R. Schreiber and C. Van Loan.
                    //
                    if (qrows >= 2 * ablas.ablascomplexblocksize(a))
                    {

                        //
                        // Prepare block reflector
                        //
                        cmatrixblockreflector(ref tmpa, ref taubuf, false, columnscount, blocksize, ref tmpt, ref work);

                        //
                        // Multiply the rest of A by Q'.
                        //
                        // Q'  = E + Y*T'*Y'  = E + TmpA'*TmpT'*TmpA
                        //
                        ablas.cmatrixgemm(qrows, blocksize, columnscount, 1.0, q, 0, blockstart, 0, tmpa, 0, 0, 2, 0.0, ref tmpr, 0, 0);
                        ablas.cmatrixgemm(qrows, blocksize, blocksize, 1.0, tmpr, 0, 0, 0, tmpt, 0, 0, 2, 0.0, ref tmpr, 0, blocksize);
                        ablas.cmatrixgemm(qrows, columnscount, blocksize, 1.0, tmpr, 0, blocksize, 0, tmpa, 0, 0, 0, 1.0, ref q, 0, blockstart);
                    }
                    else
                    {

                        //
                        // Level 2 algorithm
                        //
                        for (i = blocksize - 1; i >= 0; i--)
                        {
                            i1_ = (i) - (1);
                            for (i_ = 1; i_ <= columnscount - i; i_++)
                            {
                                t[i_] = math.conj(tmpa[i, i_ + i1_]);
                            }
                            t[1] = 1;
                            creflections.complexapplyreflectionfromtheright(ref q, math.conj(taubuf[i]), ref t, 0, qrows - 1, blockstart + i, n - 1, ref work);
                        }
                    }

                    //
                    // Advance
                    //
                    blockstart = blockstart - ablas.ablascomplexblocksize(a);
                    blocksize = ablas.ablascomplexblocksize(a);
                }
            }


            /*************************************************************************
            Unpacking of matrix L from the LQ decomposition of a matrix A

            Input parameters:
                A       -   matrices Q and L in compact form.
                            Output of CMatrixLQ subroutine.
                M       -   number of rows in given matrix A. M>=0.
                N       -   number of columns in given matrix A. N>=0.

            Output parameters:
                L       -   matrix L, array[0..M-1, 0..N-1].

              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void cmatrixlqunpackl(complex[,] a,
                int m,
                int n,
                ref complex[,] l)
            {
                int i = 0;
                int k = 0;
                int i_ = 0;

                l = new complex[0, 0];

                if (m <= 0 | n <= 0)
                {
                    return;
                }
                l = new complex[m, n];
                for (i = 0; i <= n - 1; i++)
                {
                    l[0, i] = 0;
                }
                for (i = 1; i <= m - 1; i++)
                {
                    for (i_ = 0; i_ <= n - 1; i_++)
                    {
                        l[i, i_] = l[0, i_];
                    }
                }
                for (i = 0; i <= m - 1; i++)
                {
                    k = Math.Min(i, n - 1);
                    for (i_ = 0; i_ <= k; i_++)
                    {
                        l[i, i_] = a[i, i_];
                    }
                }
            }


            /*************************************************************************
            Reduction of a rectangular matrix to  bidiagonal form

            The algorithm reduces the rectangular matrix A to  bidiagonal form by
            orthogonal transformations P and Q: A = Q*B*P.

            Input parameters:
                A       -   source matrix. array[0..M-1, 0..N-1]
                M       -   number of rows in matrix A.
                N       -   number of columns in matrix A.

            Output parameters:
                A       -   matrices Q, B, P in compact form (see below).
                TauQ    -   scalar factors which are used to form matrix Q.
                TauP    -   scalar factors which are used to form matrix P.

            The main diagonal and one of the  secondary  diagonals  of  matrix  A  are
            replaced with bidiagonal  matrix  B.  Other  elements  contain  elementary
            reflections which form MxM matrix Q and NxN matrix P, respectively.

            If M>=N, B is the upper  bidiagonal  MxN  matrix  and  is  stored  in  the
            corresponding  elements  of  matrix  A.  Matrix  Q  is  represented  as  a
            product   of   elementary   reflections   Q = H(0)*H(1)*...*H(n-1),  where
            H(i) = 1-tau*v*v'. Here tau is a scalar which is stored  in  TauQ[i],  and
            vector v has the following  structure:  v(0:i-1)=0, v(i)=1, v(i+1:m-1)  is
            stored   in   elements   A(i+1:m-1,i).   Matrix   P  is  as  follows:  P =
            G(0)*G(1)*...*G(n-2), where G(i) = 1 - tau*u*u'. Tau is stored in TauP[i],
            u(0:i)=0, u(i+1)=1, u(i+2:n-1) is stored in elements A(i,i+2:n-1).

            If M<N, B is the  lower  bidiagonal  MxN  matrix  and  is  stored  in  the
            corresponding   elements  of  matrix  A.  Q = H(0)*H(1)*...*H(m-2),  where
            H(i) = 1 - tau*v*v', tau is stored in TauQ, v(0:i)=0, v(i+1)=1, v(i+2:m-1)
            is    stored    in   elements   A(i+2:m-1,i).    P = G(0)*G(1)*...*G(m-1),
            G(i) = 1-tau*u*u', tau is stored in  TauP,  u(0:i-1)=0, u(i)=1, u(i+1:n-1)
            is stored in A(i,i+1:n-1).

            EXAMPLE:

            m=6, n=5 (m > n):               m=5, n=6 (m < n):

            (  d   e   u1  u1  u1 )         (  d   u1  u1  u1  u1  u1 )
            (  v1  d   e   u2  u2 )         (  e   d   u2  u2  u2  u2 )
            (  v1  v2  d   e   u3 )         (  v1  e   d   u3  u3  u3 )
            (  v1  v2  v3  d   e  )         (  v1  v2  e   d   u4  u4 )
            (  v1  v2  v3  v4  d  )         (  v1  v2  v3  e   d   u5 )
            (  v1  v2  v3  v4  v5 )

            Here vi and ui are vectors which form H(i) and G(i), and d and e -
            are the diagonal and off-diagonal elements of matrix B.

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994.
                 Sergey Bochkanov, ALGLIB project, translation from FORTRAN to
                 pseudocode, 2007-2010.
            *************************************************************************/
            public static void rmatrixbd(ref double[,] a,
                int m,
                int n,
                ref double[] tauq,
                ref double[] taup)
            {
                double[] work = new double[0];
                double[] t = new double[0];
                int minmn = 0;
                int maxmn = 0;
                int i = 0;
                double ltau = 0;
                int i_ = 0;
                int i1_ = 0;

                tauq = new double[0];
                taup = new double[0];


                //
                // Prepare
                //
                if (n <= 0 | m <= 0)
                {
                    return;
                }
                minmn = Math.Min(m, n);
                maxmn = Math.Max(m, n);
                work = new double[maxmn + 1];
                t = new double[maxmn + 1];
                if (m >= n)
                {
                    tauq = new double[n];
                    taup = new double[n];
                }
                else
                {
                    tauq = new double[m];
                    taup = new double[m];
                }
                if (m >= n)
                {

                    //
                    // Reduce to upper bidiagonal form
                    //
                    for (i = 0; i <= n - 1; i++)
                    {

                        //
                        // Generate elementary reflector H(i) to annihilate A(i+1:m-1,i)
                        //
                        i1_ = (i) - (1);
                        for (i_ = 1; i_ <= m - i; i_++)
                        {
                            t[i_] = a[i_ + i1_, i];
                        }
                        reflections.generatereflection(ref t, m - i, ref ltau);
                        tauq[i] = ltau;
                        i1_ = (1) - (i);
                        for (i_ = i; i_ <= m - 1; i_++)
                        {
                            a[i_, i] = t[i_ + i1_];
                        }
                        t[1] = 1;

                        //
                        // Apply H(i) to A(i:m-1,i+1:n-1) from the left
                        //
                        reflections.applyreflectionfromtheleft(ref a, ltau, t, i, m - 1, i + 1, n - 1, ref work);
                        if (i < n - 1)
                        {

                            //
                            // Generate elementary reflector G(i) to annihilate
                            // A(i,i+2:n-1)
                            //
                            i1_ = (i + 1) - (1);
                            for (i_ = 1; i_ <= n - i - 1; i_++)
                            {
                                t[i_] = a[i, i_ + i1_];
                            }
                            reflections.generatereflection(ref t, n - 1 - i, ref ltau);
                            taup[i] = ltau;
                            i1_ = (1) - (i + 1);
                            for (i_ = i + 1; i_ <= n - 1; i_++)
                            {
                                a[i, i_] = t[i_ + i1_];
                            }
                            t[1] = 1;

                            //
                            // Apply G(i) to A(i+1:m-1,i+1:n-1) from the right
                            //
                            reflections.applyreflectionfromtheright(ref a, ltau, t, i + 1, m - 1, i + 1, n - 1, ref work);
                        }
                        else
                        {
                            taup[i] = 0;
                        }
                    }
                }
                else
                {

                    //
                    // Reduce to lower bidiagonal form
                    //
                    for (i = 0; i <= m - 1; i++)
                    {

                        //
                        // Generate elementary reflector G(i) to annihilate A(i,i+1:n-1)
                        //
                        i1_ = (i) - (1);
                        for (i_ = 1; i_ <= n - i; i_++)
                        {
                            t[i_] = a[i, i_ + i1_];
                        }
                        reflections.generatereflection(ref t, n - i, ref ltau);
                        taup[i] = ltau;
                        i1_ = (1) - (i);
                        for (i_ = i; i_ <= n - 1; i_++)
                        {
                            a[i, i_] = t[i_ + i1_];
                        }
                        t[1] = 1;

                        //
                        // Apply G(i) to A(i+1:m-1,i:n-1) from the right
                        //
                        reflections.applyreflectionfromtheright(ref a, ltau, t, i + 1, m - 1, i, n - 1, ref work);
                        if (i < m - 1)
                        {

                            //
                            // Generate elementary reflector H(i) to annihilate
                            // A(i+2:m-1,i)
                            //
                            i1_ = (i + 1) - (1);
                            for (i_ = 1; i_ <= m - 1 - i; i_++)
                            {
                                t[i_] = a[i_ + i1_, i];
                            }
                            reflections.generatereflection(ref t, m - 1 - i, ref ltau);
                            tauq[i] = ltau;
                            i1_ = (1) - (i + 1);
                            for (i_ = i + 1; i_ <= m - 1; i_++)
                            {
                                a[i_, i] = t[i_ + i1_];
                            }
                            t[1] = 1;

                            //
                            // Apply H(i) to A(i+1:m-1,i+1:n-1) from the left
                            //
                            reflections.applyreflectionfromtheleft(ref a, ltau, t, i + 1, m - 1, i + 1, n - 1, ref work);
                        }
                        else
                        {
                            tauq[i] = 0;
                        }
                    }
                }
            }


            /*************************************************************************
            Unpacking matrix Q which reduces a matrix to bidiagonal form.

            Input parameters:
                QP          -   matrices Q and P in compact form.
                                Output of ToBidiagonal subroutine.
                M           -   number of rows in matrix A.
                N           -   number of columns in matrix A.
                TAUQ        -   scalar factors which are used to form Q.
                                Output of ToBidiagonal subroutine.
                QColumns    -   required number of columns in matrix Q.
                                M>=QColumns>=0.

            Output parameters:
                Q           -   first QColumns columns of matrix Q.
                                Array[0..M-1, 0..QColumns-1]
                                If QColumns=0, the array is not modified.

              -- ALGLIB --
                 2005-2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixbdunpackq(double[,] qp,
                int m,
                int n,
                double[] tauq,
                int qcolumns,
                ref double[,] q)
            {
                int i = 0;
                int j = 0;

                q = new double[0, 0];

                ap.assert(qcolumns <= m, "RMatrixBDUnpackQ: QColumns>M!");
                ap.assert(qcolumns >= 0, "RMatrixBDUnpackQ: QColumns<0!");
                if ((m == 0 | n == 0) | qcolumns == 0)
                {
                    return;
                }

                //
                // prepare Q
                //
                q = new double[m, qcolumns];
                for (i = 0; i <= m - 1; i++)
                {
                    for (j = 0; j <= qcolumns - 1; j++)
                    {
                        if (i == j)
                        {
                            q[i, j] = 1;
                        }
                        else
                        {
                            q[i, j] = 0;
                        }
                    }
                }

                //
                // Calculate
                //
                rmatrixbdmultiplybyq(qp, m, n, tauq, ref q, m, qcolumns, false, false);
            }


            /*************************************************************************
            Multiplication by matrix Q which reduces matrix A to  bidiagonal form.

            The algorithm allows pre- or post-multiply by Q or Q'.

            Input parameters:
                QP          -   matrices Q and P in compact form.
                                Output of ToBidiagonal subroutine.
                M           -   number of rows in matrix A.
                N           -   number of columns in matrix A.
                TAUQ        -   scalar factors which are used to form Q.
                                Output of ToBidiagonal subroutine.
                Z           -   multiplied matrix.
                                array[0..ZRows-1,0..ZColumns-1]
                ZRows       -   number of rows in matrix Z. If FromTheRight=False,
                                ZRows=M, otherwise ZRows can be arbitrary.
                ZColumns    -   number of columns in matrix Z. If FromTheRight=True,
                                ZColumns=M, otherwise ZColumns can be arbitrary.
                FromTheRight -  pre- or post-multiply.
                DoTranspose -   multiply by Q or Q'.

            Output parameters:
                Z           -   product of Z and Q.
                                Array[0..ZRows-1,0..ZColumns-1]
                                If ZRows=0 or ZColumns=0, the array is not modified.

              -- ALGLIB --
                 2005-2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixbdmultiplybyq(double[,] qp,
                int m,
                int n,
                double[] tauq,
                ref double[,] z,
                int zrows,
                int zcolumns,
                bool fromtheright,
                bool dotranspose)
            {
                int i = 0;
                int i1 = 0;
                int i2 = 0;
                int istep = 0;
                double[] v = new double[0];
                double[] work = new double[0];
                int mx = 0;
                int i_ = 0;
                int i1_ = 0;

                if (((m <= 0 | n <= 0) | zrows <= 0) | zcolumns <= 0)
                {
                    return;
                }
                ap.assert((fromtheright & zcolumns == m) | (!fromtheright & zrows == m), "RMatrixBDMultiplyByQ: incorrect Z size!");

                //
                // init
                //
                mx = Math.Max(m, n);
                mx = Math.Max(mx, zrows);
                mx = Math.Max(mx, zcolumns);
                v = new double[mx + 1];
                work = new double[mx + 1];
                if (m >= n)
                {

                    //
                    // setup
                    //
                    if (fromtheright)
                    {
                        i1 = 0;
                        i2 = n - 1;
                        istep = 1;
                    }
                    else
                    {
                        i1 = n - 1;
                        i2 = 0;
                        istep = -1;
                    }
                    if (dotranspose)
                    {
                        i = i1;
                        i1 = i2;
                        i2 = i;
                        istep = -istep;
                    }

                    //
                    // Process
                    //
                    i = i1;
                    do
                    {
                        i1_ = (i) - (1);
                        for (i_ = 1; i_ <= m - i; i_++)
                        {
                            v[i_] = qp[i_ + i1_, i];
                        }
                        v[1] = 1;
                        if (fromtheright)
                        {
                            reflections.applyreflectionfromtheright(ref z, tauq[i], v, 0, zrows - 1, i, m - 1, ref work);
                        }
                        else
                        {
                            reflections.applyreflectionfromtheleft(ref z, tauq[i], v, i, m - 1, 0, zcolumns - 1, ref work);
                        }
                        i = i + istep;
                    }
                    while (i != i2 + istep);
                }
                else
                {

                    //
                    // setup
                    //
                    if (fromtheright)
                    {
                        i1 = 0;
                        i2 = m - 2;
                        istep = 1;
                    }
                    else
                    {
                        i1 = m - 2;
                        i2 = 0;
                        istep = -1;
                    }
                    if (dotranspose)
                    {
                        i = i1;
                        i1 = i2;
                        i2 = i;
                        istep = -istep;
                    }

                    //
                    // Process
                    //
                    if (m - 1 > 0)
                    {
                        i = i1;
                        do
                        {
                            i1_ = (i + 1) - (1);
                            for (i_ = 1; i_ <= m - i - 1; i_++)
                            {
                                v[i_] = qp[i_ + i1_, i];
                            }
                            v[1] = 1;
                            if (fromtheright)
                            {
                                reflections.applyreflectionfromtheright(ref z, tauq[i], v, 0, zrows - 1, i + 1, m - 1, ref work);
                            }
                            else
                            {
                                reflections.applyreflectionfromtheleft(ref z, tauq[i], v, i + 1, m - 1, 0, zcolumns - 1, ref work);
                            }
                            i = i + istep;
                        }
                        while (i != i2 + istep);
                    }
                }
            }


            /*************************************************************************
            Unpacking matrix P which reduces matrix A to bidiagonal form.
            The subroutine returns transposed matrix P.

            Input parameters:
                QP      -   matrices Q and P in compact form.
                            Output of ToBidiagonal subroutine.
                M       -   number of rows in matrix A.
                N       -   number of columns in matrix A.
                TAUP    -   scalar factors which are used to form P.
                            Output of ToBidiagonal subroutine.
                PTRows  -   required number of rows of matrix P^T. N >= PTRows >= 0.

            Output parameters:
                PT      -   first PTRows columns of matrix P^T
                            Array[0..PTRows-1, 0..N-1]
                            If PTRows=0, the array is not modified.

              -- ALGLIB --
                 2005-2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixbdunpackpt(double[,] qp,
                int m,
                int n,
                double[] taup,
                int ptrows,
                ref double[,] pt)
            {
                int i = 0;
                int j = 0;

                pt = new double[0, 0];

                ap.assert(ptrows <= n, "RMatrixBDUnpackPT: PTRows>N!");
                ap.assert(ptrows >= 0, "RMatrixBDUnpackPT: PTRows<0!");
                if ((m == 0 | n == 0) | ptrows == 0)
                {
                    return;
                }

                //
                // prepare PT
                //
                pt = new double[ptrows, n];
                for (i = 0; i <= ptrows - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (i == j)
                        {
                            pt[i, j] = 1;
                        }
                        else
                        {
                            pt[i, j] = 0;
                        }
                    }
                }

                //
                // Calculate
                //
                rmatrixbdmultiplybyp(qp, m, n, taup, ref pt, ptrows, n, true, true);
            }


            /*************************************************************************
            Multiplication by matrix P which reduces matrix A to  bidiagonal form.

            The algorithm allows pre- or post-multiply by P or P'.

            Input parameters:
                QP          -   matrices Q and P in compact form.
                                Output of RMatrixBD subroutine.
                M           -   number of rows in matrix A.
                N           -   number of columns in matrix A.
                TAUP        -   scalar factors which are used to form P.
                                Output of RMatrixBD subroutine.
                Z           -   multiplied matrix.
                                Array whose indexes range within [0..ZRows-1,0..ZColumns-1].
                ZRows       -   number of rows in matrix Z. If FromTheRight=False,
                                ZRows=N, otherwise ZRows can be arbitrary.
                ZColumns    -   number of columns in matrix Z. If FromTheRight=True,
                                ZColumns=N, otherwise ZColumns can be arbitrary.
                FromTheRight -  pre- or post-multiply.
                DoTranspose -   multiply by P or P'.

            Output parameters:
                Z - product of Z and P.
                            Array whose indexes range within [0..ZRows-1,0..ZColumns-1].
                            If ZRows=0 or ZColumns=0, the array is not modified.

              -- ALGLIB --
                 2005-2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixbdmultiplybyp(double[,] qp,
                int m,
                int n,
                double[] taup,
                ref double[,] z,
                int zrows,
                int zcolumns,
                bool fromtheright,
                bool dotranspose)
            {
                int i = 0;
                double[] v = new double[0];
                double[] work = new double[0];
                int mx = 0;
                int i1 = 0;
                int i2 = 0;
                int istep = 0;
                int i_ = 0;
                int i1_ = 0;

                if (((m <= 0 | n <= 0) | zrows <= 0) | zcolumns <= 0)
                {
                    return;
                }
                ap.assert((fromtheright & zcolumns == n) | (!fromtheright & zrows == n), "RMatrixBDMultiplyByP: incorrect Z size!");

                //
                // init
                //
                mx = Math.Max(m, n);
                mx = Math.Max(mx, zrows);
                mx = Math.Max(mx, zcolumns);
                v = new double[mx + 1];
                work = new double[mx + 1];
                if (m >= n)
                {

                    //
                    // setup
                    //
                    if (fromtheright)
                    {
                        i1 = n - 2;
                        i2 = 0;
                        istep = -1;
                    }
                    else
                    {
                        i1 = 0;
                        i2 = n - 2;
                        istep = 1;
                    }
                    if (!dotranspose)
                    {
                        i = i1;
                        i1 = i2;
                        i2 = i;
                        istep = -istep;
                    }

                    //
                    // Process
                    //
                    if (n - 1 > 0)
                    {
                        i = i1;
                        do
                        {
                            i1_ = (i + 1) - (1);
                            for (i_ = 1; i_ <= n - 1 - i; i_++)
                            {
                                v[i_] = qp[i, i_ + i1_];
                            }
                            v[1] = 1;
                            if (fromtheright)
                            {
                                reflections.applyreflectionfromtheright(ref z, taup[i], v, 0, zrows - 1, i + 1, n - 1, ref work);
                            }
                            else
                            {
                                reflections.applyreflectionfromtheleft(ref z, taup[i], v, i + 1, n - 1, 0, zcolumns - 1, ref work);
                            }
                            i = i + istep;
                        }
                        while (i != i2 + istep);
                    }
                }
                else
                {

                    //
                    // setup
                    //
                    if (fromtheright)
                    {
                        i1 = m - 1;
                        i2 = 0;
                        istep = -1;
                    }
                    else
                    {
                        i1 = 0;
                        i2 = m - 1;
                        istep = 1;
                    }
                    if (!dotranspose)
                    {
                        i = i1;
                        i1 = i2;
                        i2 = i;
                        istep = -istep;
                    }

                    //
                    // Process
                    //
                    i = i1;
                    do
                    {
                        i1_ = (i) - (1);
                        for (i_ = 1; i_ <= n - i; i_++)
                        {
                            v[i_] = qp[i, i_ + i1_];
                        }
                        v[1] = 1;
                        if (fromtheright)
                        {
                            reflections.applyreflectionfromtheright(ref z, taup[i], v, 0, zrows - 1, i, n - 1, ref work);
                        }
                        else
                        {
                            reflections.applyreflectionfromtheleft(ref z, taup[i], v, i, n - 1, 0, zcolumns - 1, ref work);
                        }
                        i = i + istep;
                    }
                    while (i != i2 + istep);
                }
            }


            /*************************************************************************
            Unpacking of the main and secondary diagonals of bidiagonal decomposition
            of matrix A.

            Input parameters:
                B   -   output of RMatrixBD subroutine.
                M   -   number of rows in matrix B.
                N   -   number of columns in matrix B.

            Output parameters:
                IsUpper -   True, if the matrix is upper bidiagonal.
                            otherwise IsUpper is False.
                D       -   the main diagonal.
                            Array whose index ranges within [0..Min(M,N)-1].
                E       -   the secondary diagonal (upper or lower, depending on
                            the value of IsUpper).
                            Array index ranges within [0..Min(M,N)-1], the last
                            element is not used.

              -- ALGLIB --
                 2005-2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixbdunpackdiagonals(double[,] b,
                int m,
                int n,
                ref bool isupper,
                ref double[] d,
                ref double[] e)
            {
                int i = 0;

                isupper = new bool();
                d = new double[0];
                e = new double[0];

                isupper = m >= n;
                if (m <= 0 | n <= 0)
                {
                    return;
                }
                if (isupper)
                {
                    d = new double[n];
                    e = new double[n];
                    for (i = 0; i <= n - 2; i++)
                    {
                        d[i] = b[i, i];
                        e[i] = b[i, i + 1];
                    }
                    d[n - 1] = b[n - 1, n - 1];
                }
                else
                {
                    d = new double[m];
                    e = new double[m];
                    for (i = 0; i <= m - 2; i++)
                    {
                        d[i] = b[i, i];
                        e[i] = b[i + 1, i];
                    }
                    d[m - 1] = b[m - 1, m - 1];
                }
            }


            /*************************************************************************
            Reduction of a square matrix to  upper Hessenberg form: Q'*A*Q = H,
            where Q is an orthogonal matrix, H - Hessenberg matrix.

            Input parameters:
                A       -   matrix A with elements [0..N-1, 0..N-1]
                N       -   size of matrix A.

            Output parameters:
                A       -   matrices Q and P in  compact form (see below).
                Tau     -   array of scalar factors which are used to form matrix Q.
                            Array whose index ranges within [0..N-2]

            Matrix H is located on the main diagonal, on the lower secondary  diagonal
            and above the main diagonal of matrix A. The elements which are used to
            form matrix Q are situated in array Tau and below the lower secondary
            diagonal of matrix A as follows:

            Matrix Q is represented as a product of elementary reflections

            Q = H(0)*H(2)*...*H(n-2),

            where each H(i) is given by

            H(i) = 1 - tau * v * (v^T)

            where tau is a scalar stored in Tau[I]; v - is a real vector,
            so that v(0:i) = 0, v(i+1) = 1, v(i+2:n-1) stored in A(i+2:n-1,i).

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 October 31, 1992
            *************************************************************************/
            public static void rmatrixhessenberg(ref double[,] a,
                int n,
                ref double[] tau)
            {
                int i = 0;
                double v = 0;
                double[] t = new double[0];
                double[] work = new double[0];
                int i_ = 0;
                int i1_ = 0;

                tau = new double[0];

                ap.assert(n >= 0, "RMatrixHessenberg: incorrect N!");

                //
                // Quick return if possible
                //
                if (n <= 1)
                {
                    return;
                }
                tau = new double[n - 2 + 1];
                t = new double[n + 1];
                work = new double[n - 1 + 1];
                for (i = 0; i <= n - 2; i++)
                {

                    //
                    // Compute elementary reflector H(i) to annihilate A(i+2:ihi,i)
                    //
                    i1_ = (i + 1) - (1);
                    for (i_ = 1; i_ <= n - i - 1; i_++)
                    {
                        t[i_] = a[i_ + i1_, i];
                    }
                    reflections.generatereflection(ref t, n - i - 1, ref v);
                    i1_ = (1) - (i + 1);
                    for (i_ = i + 1; i_ <= n - 1; i_++)
                    {
                        a[i_, i] = t[i_ + i1_];
                    }
                    tau[i] = v;
                    t[1] = 1;

                    //
                    // Apply H(i) to A(1:ihi,i+1:ihi) from the right
                    //
                    reflections.applyreflectionfromtheright(ref a, v, t, 0, n - 1, i + 1, n - 1, ref work);

                    //
                    // Apply H(i) to A(i+1:ihi,i+1:n) from the left
                    //
                    reflections.applyreflectionfromtheleft(ref a, v, t, i + 1, n - 1, i + 1, n - 1, ref work);
                }
            }


            /*************************************************************************
            Unpacking matrix Q which reduces matrix A to upper Hessenberg form

            Input parameters:
                A   -   output of RMatrixHessenberg subroutine.
                N   -   size of matrix A.
                Tau -   scalar factors which are used to form Q.
                        Output of RMatrixHessenberg subroutine.

            Output parameters:
                Q   -   matrix Q.
                        Array whose indexes range within [0..N-1, 0..N-1].

              -- ALGLIB --
                 2005-2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixhessenbergunpackq(double[,] a,
                int n,
                double[] tau,
                ref double[,] q)
            {
                int i = 0;
                int j = 0;
                double[] v = new double[0];
                double[] work = new double[0];
                int i_ = 0;
                int i1_ = 0;

                q = new double[0, 0];

                if (n == 0)
                {
                    return;
                }

                //
                // init
                //
                q = new double[n - 1 + 1, n - 1 + 1];
                v = new double[n - 1 + 1];
                work = new double[n - 1 + 1];
                for (i = 0; i <= n - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (i == j)
                        {
                            q[i, j] = 1;
                        }
                        else
                        {
                            q[i, j] = 0;
                        }
                    }
                }

                //
                // unpack Q
                //
                for (i = 0; i <= n - 2; i++)
                {

                    //
                    // Apply H(i)
                    //
                    i1_ = (i + 1) - (1);
                    for (i_ = 1; i_ <= n - i - 1; i_++)
                    {
                        v[i_] = a[i_ + i1_, i];
                    }
                    v[1] = 1;
                    reflections.applyreflectionfromtheright(ref q, tau[i], v, 0, n - 1, i + 1, n - 1, ref work);
                }
            }


            /*************************************************************************
            Unpacking matrix H (the result of matrix A reduction to upper Hessenberg form)

            Input parameters:
                A   -   output of RMatrixHessenberg subroutine.
                N   -   size of matrix A.

            Output parameters:
                H   -   matrix H. Array whose indexes range within [0..N-1, 0..N-1].

              -- ALGLIB --
                 2005-2010
                 Bochkanov Sergey
            *************************************************************************/
            public static void rmatrixhessenbergunpackh(double[,] a,
                int n,
                ref double[,] h)
            {
                int i = 0;
                int j = 0;
                double[] v = new double[0];
                double[] work = new double[0];
                int i_ = 0;

                h = new double[0, 0];

                if (n == 0)
                {
                    return;
                }
                h = new double[n - 1 + 1, n - 1 + 1];
                for (i = 0; i <= n - 1; i++)
                {
                    for (j = 0; j <= i - 2; j++)
                    {
                        h[i, j] = 0;
                    }
                    j = Math.Max(0, i - 1);
                    for (i_ = j; i_ <= n - 1; i_++)
                    {
                        h[i, i_] = a[i, i_];
                    }
                }
            }


            /*************************************************************************
            Reduction of a symmetric matrix which is given by its higher or lower
            triangular part to a tridiagonal matrix using orthogonal similarity
            transformation: Q'*A*Q=T.

            Input parameters:
                A       -   matrix to be transformed
                            array with elements [0..N-1, 0..N-1].
                N       -   size of matrix A.
                IsUpper -   storage format. If IsUpper = True, then matrix A is given
                            by its upper triangle, and the lower triangle is not used
                            and not modified by the algorithm, and vice versa
                            if IsUpper = False.

            Output parameters:
                A       -   matrices T and Q in  compact form (see lower)
                Tau     -   array of factors which are forming matrices H(i)
                            array with elements [0..N-2].
                D       -   main diagonal of symmetric matrix T.
                            array with elements [0..N-1].
                E       -   secondary diagonal of symmetric matrix T.
                            array with elements [0..N-2].


              If IsUpper=True, the matrix Q is represented as a product of elementary
              reflectors

                 Q = H(n-2) . . . H(2) H(0).

              Each H(i) has the form

                 H(i) = I - tau * v * v'

              where tau is a real scalar, and v is a real vector with
              v(i+1:n-1) = 0, v(i) = 1, v(0:i-1) is stored on exit in
              A(0:i-1,i+1), and tau in TAU(i).

              If IsUpper=False, the matrix Q is represented as a product of elementary
              reflectors

                 Q = H(0) H(2) . . . H(n-2).

              Each H(i) has the form

                 H(i) = I - tau * v * v'

              where tau is a real scalar, and v is a real vector with
              v(0:i) = 0, v(i+1) = 1, v(i+2:n-1) is stored on exit in A(i+2:n-1,i),
              and tau in TAU(i).

              The contents of A on exit are illustrated by the following examples
              with n = 5:

              if UPLO = 'U':                       if UPLO = 'L':

                (  d   e   v1  v2  v3 )              (  d                  )
                (      d   e   v2  v3 )              (  e   d              )
                (          d   e   v3 )              (  v0  e   d          )
                (              d   e  )              (  v0  v1  e   d      )
                (                  d  )              (  v0  v1  v2  e   d  )

              where d and e denote diagonal and off-diagonal elements of T, and vi
              denotes an element of the vector defining H(i).

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 October 31, 1992
            *************************************************************************/
            public static void smatrixtd(ref double[,] a,
                int n,
                bool isupper,
                ref double[] tau,
                ref double[] d,
                ref double[] e)
            {
                int i = 0;
                double alpha = 0;
                double taui = 0;
                double v = 0;
                double[] t = new double[0];
                double[] t2 = new double[0];
                double[] t3 = new double[0];
                int i_ = 0;
                int i1_ = 0;

                tau = new double[0];
                d = new double[0];
                e = new double[0];

                if (n <= 0)
                {
                    return;
                }
                t = new double[n + 1];
                t2 = new double[n + 1];
                t3 = new double[n + 1];
                if (n > 1)
                {
                    tau = new double[n - 2 + 1];
                }
                d = new double[n - 1 + 1];
                if (n > 1)
                {
                    e = new double[n - 2 + 1];
                }
                if (isupper)
                {

                    //
                    // Reduce the upper triangle of A
                    //
                    for (i = n - 2; i >= 0; i--)
                    {

                        //
                        // Generate elementary reflector H() = E - tau * v * v'
                        //
                        if (i >= 1)
                        {
                            i1_ = (0) - (2);
                            for (i_ = 2; i_ <= i + 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i + 1];
                            }
                        }
                        t[1] = a[i, i + 1];
                        reflections.generatereflection(ref t, i + 1, ref taui);
                        if (i >= 1)
                        {
                            i1_ = (2) - (0);
                            for (i_ = 0; i_ <= i - 1; i_++)
                            {
                                a[i_, i + 1] = t[i_ + i1_];
                            }
                        }
                        a[i, i + 1] = t[1];
                        e[i] = a[i, i + 1];
                        if ((double)(taui) != (double)(0))
                        {

                            //
                            // Apply H from both sides to A
                            //
                            a[i, i + 1] = 1;

                            //
                            // Compute  x := tau * A * v  storing x in TAU
                            //
                            i1_ = (0) - (1);
                            for (i_ = 1; i_ <= i + 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i + 1];
                            }
                            sblas.symmetricmatrixvectormultiply(a, isupper, 0, i, t, taui, ref t3);
                            i1_ = (1) - (0);
                            for (i_ = 0; i_ <= i; i_++)
                            {
                                tau[i_] = t3[i_ + i1_];
                            }

                            //
                            // Compute  w := x - 1/2 * tau * (x'*v) * v
                            //
                            v = 0.0;
                            for (i_ = 0; i_ <= i; i_++)
                            {
                                v += tau[i_] * a[i_, i + 1];
                            }
                            alpha = -(0.5 * taui * v);
                            for (i_ = 0; i_ <= i; i_++)
                            {
                                tau[i_] = tau[i_] + alpha * a[i_, i + 1];
                            }

                            //
                            // Apply the transformation as a rank-2 update:
                            //    A := A - v * w' - w * v'
                            //
                            i1_ = (0) - (1);
                            for (i_ = 1; i_ <= i + 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i + 1];
                            }
                            i1_ = (0) - (1);
                            for (i_ = 1; i_ <= i + 1; i_++)
                            {
                                t3[i_] = tau[i_ + i1_];
                            }
                            sblas.symmetricrank2update(ref a, isupper, 0, i, t, t3, ref t2, -1);
                            a[i, i + 1] = e[i];
                        }
                        d[i + 1] = a[i + 1, i + 1];
                        tau[i] = taui;
                    }
                    d[0] = a[0, 0];
                }
                else
                {

                    //
                    // Reduce the lower triangle of A
                    //
                    for (i = 0; i <= n - 2; i++)
                    {

                        //
                        // Generate elementary reflector H = E - tau * v * v'
                        //
                        i1_ = (i + 1) - (1);
                        for (i_ = 1; i_ <= n - i - 1; i_++)
                        {
                            t[i_] = a[i_ + i1_, i];
                        }
                        reflections.generatereflection(ref t, n - i - 1, ref taui);
                        i1_ = (1) - (i + 1);
                        for (i_ = i + 1; i_ <= n - 1; i_++)
                        {
                            a[i_, i] = t[i_ + i1_];
                        }
                        e[i] = a[i + 1, i];
                        if ((double)(taui) != (double)(0))
                        {

                            //
                            // Apply H from both sides to A
                            //
                            a[i + 1, i] = 1;

                            //
                            // Compute  x := tau * A * v  storing y in TAU
                            //
                            i1_ = (i + 1) - (1);
                            for (i_ = 1; i_ <= n - i - 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i];
                            }
                            sblas.symmetricmatrixvectormultiply(a, isupper, i + 1, n - 1, t, taui, ref t2);
                            i1_ = (1) - (i);
                            for (i_ = i; i_ <= n - 2; i_++)
                            {
                                tau[i_] = t2[i_ + i1_];
                            }

                            //
                            // Compute  w := x - 1/2 * tau * (x'*v) * v
                            //
                            i1_ = (i + 1) - (i);
                            v = 0.0;
                            for (i_ = i; i_ <= n - 2; i_++)
                            {
                                v += tau[i_] * a[i_ + i1_, i];
                            }
                            alpha = -(0.5 * taui * v);
                            i1_ = (i + 1) - (i);
                            for (i_ = i; i_ <= n - 2; i_++)
                            {
                                tau[i_] = tau[i_] + alpha * a[i_ + i1_, i];
                            }

                            //
                            // Apply the transformation as a rank-2 update:
                            //     A := A - v * w' - w * v'
                            //
                            //
                            i1_ = (i + 1) - (1);
                            for (i_ = 1; i_ <= n - i - 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i];
                            }
                            i1_ = (i) - (1);
                            for (i_ = 1; i_ <= n - i - 1; i_++)
                            {
                                t2[i_] = tau[i_ + i1_];
                            }
                            sblas.symmetricrank2update(ref a, isupper, i + 1, n - 1, t, t2, ref t3, -1);
                            a[i + 1, i] = e[i];
                        }
                        d[i] = a[i, i];
                        tau[i] = taui;
                    }
                    d[n - 1] = a[n - 1, n - 1];
                }
            }


            /*************************************************************************
            Unpacking matrix Q which reduces symmetric matrix to a tridiagonal
            form.

            Input parameters:
                A       -   the result of a SMatrixTD subroutine
                N       -   size of matrix A.
                IsUpper -   storage format (a parameter of SMatrixTD subroutine)
                Tau     -   the result of a SMatrixTD subroutine

            Output parameters:
                Q       -   transformation matrix.
                            array with elements [0..N-1, 0..N-1].

              -- ALGLIB --
                 Copyright 2005-2010 by Bochkanov Sergey
            *************************************************************************/
            public static void smatrixtdunpackq(double[,] a,
                int n,
                bool isupper,
                double[] tau,
                ref double[,] q)
            {
                int i = 0;
                int j = 0;
                double[] v = new double[0];
                double[] work = new double[0];
                int i_ = 0;
                int i1_ = 0;

                q = new double[0, 0];

                if (n == 0)
                {
                    return;
                }

                //
                // init
                //
                q = new double[n - 1 + 1, n - 1 + 1];
                v = new double[n + 1];
                work = new double[n - 1 + 1];
                for (i = 0; i <= n - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (i == j)
                        {
                            q[i, j] = 1;
                        }
                        else
                        {
                            q[i, j] = 0;
                        }
                    }
                }

                //
                // unpack Q
                //
                if (isupper)
                {
                    for (i = 0; i <= n - 2; i++)
                    {

                        //
                        // Apply H(i)
                        //
                        i1_ = (0) - (1);
                        for (i_ = 1; i_ <= i + 1; i_++)
                        {
                            v[i_] = a[i_ + i1_, i + 1];
                        }
                        v[i + 1] = 1;
                        reflections.applyreflectionfromtheleft(ref q, tau[i], v, 0, i, 0, n - 1, ref work);
                    }
                }
                else
                {
                    for (i = n - 2; i >= 0; i--)
                    {

                        //
                        // Apply H(i)
                        //
                        i1_ = (i + 1) - (1);
                        for (i_ = 1; i_ <= n - i - 1; i_++)
                        {
                            v[i_] = a[i_ + i1_, i];
                        }
                        v[1] = 1;
                        reflections.applyreflectionfromtheleft(ref q, tau[i], v, i + 1, n - 1, 0, n - 1, ref work);
                    }
                }
            }


            /*************************************************************************
            Reduction of a Hermitian matrix which is given  by  its  higher  or  lower
            triangular part to a real  tridiagonal  matrix  using  unitary  similarity
            transformation: Q'*A*Q = T.

            Input parameters:
                A       -   matrix to be transformed
                            array with elements [0..N-1, 0..N-1].
                N       -   size of matrix A.
                IsUpper -   storage format. If IsUpper = True, then matrix A is  given
                            by its upper triangle, and the lower triangle is not  used
                            and not modified by the algorithm, and vice versa
                            if IsUpper = False.

            Output parameters:
                A       -   matrices T and Q in  compact form (see lower)
                Tau     -   array of factors which are forming matrices H(i)
                            array with elements [0..N-2].
                D       -   main diagonal of real symmetric matrix T.
                            array with elements [0..N-1].
                E       -   secondary diagonal of real symmetric matrix T.
                            array with elements [0..N-2].


              If IsUpper=True, the matrix Q is represented as a product of elementary
              reflectors

                 Q = H(n-2) . . . H(2) H(0).

              Each H(i) has the form

                 H(i) = I - tau * v * v'

              where tau is a complex scalar, and v is a complex vector with
              v(i+1:n-1) = 0, v(i) = 1, v(0:i-1) is stored on exit in
              A(0:i-1,i+1), and tau in TAU(i).

              If IsUpper=False, the matrix Q is represented as a product of elementary
              reflectors

                 Q = H(0) H(2) . . . H(n-2).

              Each H(i) has the form

                 H(i) = I - tau * v * v'

              where tau is a complex scalar, and v is a complex vector with
              v(0:i) = 0, v(i+1) = 1, v(i+2:n-1) is stored on exit in A(i+2:n-1,i),
              and tau in TAU(i).

              The contents of A on exit are illustrated by the following examples
              with n = 5:

              if UPLO = 'U':                       if UPLO = 'L':

                (  d   e   v1  v2  v3 )              (  d                  )
                (      d   e   v2  v3 )              (  e   d              )
                (          d   e   v3 )              (  v0  e   d          )
                (              d   e  )              (  v0  v1  e   d      )
                (                  d  )              (  v0  v1  v2  e   d  )

            where d and e denote diagonal and off-diagonal elements of T, and vi
            denotes an element of the vector defining H(i).

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 October 31, 1992
            *************************************************************************/
            public static void hmatrixtd(ref complex[,] a,
                int n,
                bool isupper,
                ref complex[] tau,
                ref double[] d,
                ref double[] e)
            {
                int i = 0;
                complex alpha = 0;
                complex taui = 0;
                complex v = 0;
                complex[] t = new complex[0];
                complex[] t2 = new complex[0];
                complex[] t3 = new complex[0];
                int i_ = 0;
                int i1_ = 0;

                tau = new complex[0];
                d = new double[0];
                e = new double[0];

                if (n <= 0)
                {
                    return;
                }
                for (i = 0; i <= n - 1; i++)
                {
                    ap.assert((double)(a[i, i].y) == (double)(0));
                }
                if (n > 1)
                {
                    tau = new complex[n - 2 + 1];
                    e = new double[n - 2 + 1];
                }
                d = new double[n - 1 + 1];
                t = new complex[n - 1 + 1];
                t2 = new complex[n - 1 + 1];
                t3 = new complex[n - 1 + 1];
                if (isupper)
                {

                    //
                    // Reduce the upper triangle of A
                    //
                    a[n - 1, n - 1] = a[n - 1, n - 1].x;
                    for (i = n - 2; i >= 0; i--)
                    {

                        //
                        // Generate elementary reflector H = I+1 - tau * v * v'
                        //
                        alpha = a[i, i + 1];
                        t[1] = alpha;
                        if (i >= 1)
                        {
                            i1_ = (0) - (2);
                            for (i_ = 2; i_ <= i + 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i + 1];
                            }
                        }
                        creflections.complexgeneratereflection(ref t, i + 1, ref taui);
                        if (i >= 1)
                        {
                            i1_ = (2) - (0);
                            for (i_ = 0; i_ <= i - 1; i_++)
                            {
                                a[i_, i + 1] = t[i_ + i1_];
                            }
                        }
                        alpha = t[1];
                        e[i] = alpha.x;
                        if (taui != 0)
                        {

                            //
                            // Apply H(I+1) from both sides to A
                            //
                            a[i, i + 1] = 1;

                            //
                            // Compute  x := tau * A * v  storing x in TAU
                            //
                            i1_ = (0) - (1);
                            for (i_ = 1; i_ <= i + 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i + 1];
                            }
                            hblas.hermitianmatrixvectormultiply(a, isupper, 0, i, t, taui, ref t2);
                            i1_ = (1) - (0);
                            for (i_ = 0; i_ <= i; i_++)
                            {
                                tau[i_] = t2[i_ + i1_];
                            }

                            //
                            // Compute  w := x - 1/2 * tau * (x'*v) * v
                            //
                            v = 0.0;
                            for (i_ = 0; i_ <= i; i_++)
                            {
                                v += math.conj(tau[i_]) * a[i_, i + 1];
                            }
                            alpha = -(0.5 * taui * v);
                            for (i_ = 0; i_ <= i; i_++)
                            {
                                tau[i_] = tau[i_] + alpha * a[i_, i + 1];
                            }

                            //
                            // Apply the transformation as a rank-2 update:
                            //    A := A - v * w' - w * v'
                            //
                            i1_ = (0) - (1);
                            for (i_ = 1; i_ <= i + 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i + 1];
                            }
                            i1_ = (0) - (1);
                            for (i_ = 1; i_ <= i + 1; i_++)
                            {
                                t3[i_] = tau[i_ + i1_];
                            }
                            hblas.hermitianrank2update(ref a, isupper, 0, i, t, t3, ref t2, -1);
                        }
                        else
                        {
                            a[i, i] = a[i, i].x;
                        }
                        a[i, i + 1] = e[i];
                        d[i + 1] = a[i + 1, i + 1].x;
                        tau[i] = taui;
                    }
                    d[0] = a[0, 0].x;
                }
                else
                {

                    //
                    // Reduce the lower triangle of A
                    //
                    a[0, 0] = a[0, 0].x;
                    for (i = 0; i <= n - 2; i++)
                    {

                        //
                        // Generate elementary reflector H = I - tau * v * v'
                        //
                        i1_ = (i + 1) - (1);
                        for (i_ = 1; i_ <= n - i - 1; i_++)
                        {
                            t[i_] = a[i_ + i1_, i];
                        }
                        creflections.complexgeneratereflection(ref t, n - i - 1, ref taui);
                        i1_ = (1) - (i + 1);
                        for (i_ = i + 1; i_ <= n - 1; i_++)
                        {
                            a[i_, i] = t[i_ + i1_];
                        }
                        e[i] = a[i + 1, i].x;
                        if (taui != 0)
                        {

                            //
                            // Apply H(i) from both sides to A(i+1:n,i+1:n)
                            //
                            a[i + 1, i] = 1;

                            //
                            // Compute  x := tau * A * v  storing y in TAU
                            //
                            i1_ = (i + 1) - (1);
                            for (i_ = 1; i_ <= n - i - 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i];
                            }
                            hblas.hermitianmatrixvectormultiply(a, isupper, i + 1, n - 1, t, taui, ref t2);
                            i1_ = (1) - (i);
                            for (i_ = i; i_ <= n - 2; i_++)
                            {
                                tau[i_] = t2[i_ + i1_];
                            }

                            //
                            // Compute  w := x - 1/2 * tau * (x'*v) * v
                            //
                            i1_ = (i + 1) - (i);
                            v = 0.0;
                            for (i_ = i; i_ <= n - 2; i_++)
                            {
                                v += math.conj(tau[i_]) * a[i_ + i1_, i];
                            }
                            alpha = -(0.5 * taui * v);
                            i1_ = (i + 1) - (i);
                            for (i_ = i; i_ <= n - 2; i_++)
                            {
                                tau[i_] = tau[i_] + alpha * a[i_ + i1_, i];
                            }

                            //
                            // Apply the transformation as a rank-2 update:
                            // A := A - v * w' - w * v'
                            //
                            i1_ = (i + 1) - (1);
                            for (i_ = 1; i_ <= n - i - 1; i_++)
                            {
                                t[i_] = a[i_ + i1_, i];
                            }
                            i1_ = (i) - (1);
                            for (i_ = 1; i_ <= n - i - 1; i_++)
                            {
                                t2[i_] = tau[i_ + i1_];
                            }
                            hblas.hermitianrank2update(ref a, isupper, i + 1, n - 1, t, t2, ref t3, -1);
                        }
                        else
                        {
                            a[i + 1, i + 1] = a[i + 1, i + 1].x;
                        }
                        a[i + 1, i] = e[i];
                        d[i] = a[i, i].x;
                        tau[i] = taui;
                    }
                    d[n - 1] = a[n - 1, n - 1].x;
                }
            }


            /*************************************************************************
            Unpacking matrix Q which reduces a Hermitian matrix to a real  tridiagonal
            form.

            Input parameters:
                A       -   the result of a HMatrixTD subroutine
                N       -   size of matrix A.
                IsUpper -   storage format (a parameter of HMatrixTD subroutine)
                Tau     -   the result of a HMatrixTD subroutine

            Output parameters:
                Q       -   transformation matrix.
                            array with elements [0..N-1, 0..N-1].

              -- ALGLIB --
                 Copyright 2005-2010 by Bochkanov Sergey
            *************************************************************************/
            public static void hmatrixtdunpackq(complex[,] a,
                int n,
                bool isupper,
                complex[] tau,
                ref complex[,] q)
            {
                int i = 0;
                int j = 0;
                complex[] v = new complex[0];
                complex[] work = new complex[0];
                int i_ = 0;
                int i1_ = 0;

                q = new complex[0, 0];

                if (n == 0)
                {
                    return;
                }

                //
                // init
                //
                q = new complex[n - 1 + 1, n - 1 + 1];
                v = new complex[n + 1];
                work = new complex[n - 1 + 1];
                for (i = 0; i <= n - 1; i++)
                {
                    for (j = 0; j <= n - 1; j++)
                    {
                        if (i == j)
                        {
                            q[i, j] = 1;
                        }
                        else
                        {
                            q[i, j] = 0;
                        }
                    }
                }

                //
                // unpack Q
                //
                if (isupper)
                {
                    for (i = 0; i <= n - 2; i++)
                    {

                        //
                        // Apply H(i)
                        //
                        i1_ = (0) - (1);
                        for (i_ = 1; i_ <= i + 1; i_++)
                        {
                            v[i_] = a[i_ + i1_, i + 1];
                        }
                        v[i + 1] = 1;
                        creflections.complexapplyreflectionfromtheleft(ref q, tau[i], v, 0, i, 0, n - 1, ref work);
                    }
                }
                else
                {
                    for (i = n - 2; i >= 0; i--)
                    {

                        //
                        // Apply H(i)
                        //
                        i1_ = (i + 1) - (1);
                        for (i_ = 1; i_ <= n - i - 1; i_++)
                        {
                            v[i_] = a[i_ + i1_, i];
                        }
                        v[1] = 1;
                        creflections.complexapplyreflectionfromtheleft(ref q, tau[i], v, i + 1, n - 1, 0, n - 1, ref work);
                    }
                }
            }


            /*************************************************************************
            Base case for real QR

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994.
                 Sergey Bochkanov, ALGLIB project, translation from FORTRAN to
                 pseudocode, 2007-2010.
            *************************************************************************/
            private static void rmatrixqrbasecase(ref double[,] a,
                int m,
                int n,
                ref double[] work,
                ref double[] t,
                ref double[] tau)
            {
                int i = 0;
                int k = 0;
                int minmn = 0;
                double tmp = 0;
                int i_ = 0;
                int i1_ = 0;

                minmn = Math.Min(m, n);

                //
                // Test the input arguments
                //
                k = minmn;
                for (i = 0; i <= k - 1; i++)
                {

                    //
                    // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
                    //
                    i1_ = (i) - (1);
                    for (i_ = 1; i_ <= m - i; i_++)
                    {
                        t[i_] = a[i_ + i1_, i];
                    }
                    reflections.generatereflection(ref t, m - i, ref tmp);
                    tau[i] = tmp;
                    i1_ = (1) - (i);
                    for (i_ = i; i_ <= m - 1; i_++)
                    {
                        a[i_, i] = t[i_ + i1_];
                    }
                    t[1] = 1;
                    if (i < n)
                    {

                        //
                        // Apply H(i) to A(i:m-1,i+1:n-1) from the left
                        //
                        reflections.applyreflectionfromtheleft(ref a, tau[i], t, i, m - 1, i + 1, n - 1, ref work);
                    }
                }
            }


            /*************************************************************************
            Base case for real LQ

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994.
                 Sergey Bochkanov, ALGLIB project, translation from FORTRAN to
                 pseudocode, 2007-2010.
            *************************************************************************/
            private static void rmatrixlqbasecase(ref double[,] a,
                int m,
                int n,
                ref double[] work,
                ref double[] t,
                ref double[] tau)
            {
                int i = 0;
                int k = 0;
                int minmn = 0;
                double tmp = 0;
                int i_ = 0;
                int i1_ = 0;

                minmn = Math.Min(m, n);
                k = Math.Min(m, n);
                for (i = 0; i <= k - 1; i++)
                {

                    //
                    // Generate elementary reflector H(i) to annihilate A(i,i+1:n-1)
                    //
                    i1_ = (i) - (1);
                    for (i_ = 1; i_ <= n - i; i_++)
                    {
                        t[i_] = a[i, i_ + i1_];
                    }
                    reflections.generatereflection(ref t, n - i, ref tmp);
                    tau[i] = tmp;
                    i1_ = (1) - (i);
                    for (i_ = i; i_ <= n - 1; i_++)
                    {
                        a[i, i_] = t[i_ + i1_];
                    }
                    t[1] = 1;
                    if (i < n)
                    {

                        //
                        // Apply H(i) to A(i+1:m,i:n) from the right
                        //
                        reflections.applyreflectionfromtheright(ref a, tau[i], t, i + 1, m - 1, i, n - 1, ref work);
                    }
                }
            }


            /*************************************************************************
            Base case for complex QR

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994.
                 Sergey Bochkanov, ALGLIB project, translation from FORTRAN to
                 pseudocode, 2007-2010.
            *************************************************************************/
            private static void cmatrixqrbasecase(ref complex[,] a,
                int m,
                int n,
                ref complex[] work,
                ref complex[] t,
                ref complex[] tau)
            {
                int i = 0;
                int k = 0;
                int mmi = 0;
                int minmn = 0;
                complex tmp = 0;
                int i_ = 0;
                int i1_ = 0;

                minmn = Math.Min(m, n);
                if (minmn <= 0)
                {
                    return;
                }

                //
                // Test the input arguments
                //
                k = Math.Min(m, n);
                for (i = 0; i <= k - 1; i++)
                {

                    //
                    // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
                    //
                    mmi = m - i;
                    i1_ = (i) - (1);
                    for (i_ = 1; i_ <= mmi; i_++)
                    {
                        t[i_] = a[i_ + i1_, i];
                    }
                    creflections.complexgeneratereflection(ref t, mmi, ref tmp);
                    tau[i] = tmp;
                    i1_ = (1) - (i);
                    for (i_ = i; i_ <= m - 1; i_++)
                    {
                        a[i_, i] = t[i_ + i1_];
                    }
                    t[1] = 1;
                    if (i < n - 1)
                    {

                        //
                        // Apply H'(i) to A(i:m,i+1:n) from the left
                        //
                        creflections.complexapplyreflectionfromtheleft(ref a, math.conj(tau[i]), t, i, m - 1, i + 1, n - 1, ref work);
                    }
                }
            }


            /*************************************************************************
            Base case for complex LQ

              -- LAPACK routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994.
                 Sergey Bochkanov, ALGLIB project, translation from FORTRAN to
                 pseudocode, 2007-2010.
            *************************************************************************/
            private static void cmatrixlqbasecase(ref complex[,] a,
                int m,
                int n,
                ref complex[] work,
                ref complex[] t,
                ref complex[] tau)
            {
                int i = 0;
                int minmn = 0;
                complex tmp = 0;
                int i_ = 0;
                int i1_ = 0;

                minmn = Math.Min(m, n);
                if (minmn <= 0)
                {
                    return;
                }

                //
                // Test the input arguments
                //
                for (i = 0; i <= minmn - 1; i++)
                {

                    //
                    // Generate elementary reflector H(i)
                    //
                    // NOTE: ComplexGenerateReflection() generates left reflector,
                    // i.e. H which reduces x by applyiong from the left, but we
                    // need RIGHT reflector. So we replace H=E-tau*v*v' by H^H,
                    // which changes v to conj(v).
                    //
                    i1_ = (i) - (1);
                    for (i_ = 1; i_ <= n - i; i_++)
                    {
                        t[i_] = math.conj(a[i, i_ + i1_]);
                    }
                    creflections.complexgeneratereflection(ref t, n - i, ref tmp);
                    tau[i] = tmp;
                    i1_ = (1) - (i);
                    for (i_ = i; i_ <= n - 1; i_++)
                    {
                        a[i, i_] = math.conj(t[i_ + i1_]);
                    }
                    t[1] = 1;
                    if (i < m - 1)
                    {

                        //
                        // Apply H'(i)
                        //
                        creflections.complexapplyreflectionfromtheright(ref a, tau[i], ref t, i + 1, m - 1, i, n - 1, ref work);
                    }
                }
            }


            /*************************************************************************
            Generate block reflector:
            * fill unused parts of reflectors matrix by zeros
            * fill diagonal of reflectors matrix by ones
            * generate triangular factor T

            PARAMETERS:
                A           -   either LengthA*BlockSize (if ColumnwiseA) or
                                BlockSize*LengthA (if not ColumnwiseA) matrix of
                                elementary reflectors.
                                Modified on exit.
                Tau         -   scalar factors
                ColumnwiseA -   reflectors are stored in rows or in columns
                LengthA     -   length of largest reflector
                BlockSize   -   number of reflectors
                T           -   array[BlockSize,2*BlockSize]. Left BlockSize*BlockSize
                                submatrix stores triangular factor on exit.
                WORK        -   array[BlockSize]
            
              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            private static void rmatrixblockreflector(ref double[,] a,
                ref double[] tau,
                bool columnwisea,
                int lengtha,
                int blocksize,
                ref double[,] t,
                ref double[] work)
            {
                int i = 0;
                int j = 0;
                int k = 0;
                double v = 0;
                int i_ = 0;
                int i1_ = 0;


                //
                // fill beginning of new column with zeros,
                // load 1.0 in the first non-zero element
                //
                for (k = 0; k <= blocksize - 1; k++)
                {
                    if (columnwisea)
                    {
                        for (i = 0; i <= k - 1; i++)
                        {
                            a[i, k] = 0;
                        }
                    }
                    else
                    {
                        for (i = 0; i <= k - 1; i++)
                        {
                            a[k, i] = 0;
                        }
                    }
                    a[k, k] = 1;
                }

                //
                // Calculate Gram matrix of A
                //
                for (i = 0; i <= blocksize - 1; i++)
                {
                    for (j = 0; j <= blocksize - 1; j++)
                    {
                        t[i, blocksize + j] = 0;
                    }
                }
                for (k = 0; k <= lengtha - 1; k++)
                {
                    for (j = 1; j <= blocksize - 1; j++)
                    {
                        if (columnwisea)
                        {
                            v = a[k, j];
                            if ((double)(v) != (double)(0))
                            {
                                i1_ = (0) - (blocksize);
                                for (i_ = blocksize; i_ <= blocksize + j - 1; i_++)
                                {
                                    t[j, i_] = t[j, i_] + v * a[k, i_ + i1_];
                                }
                            }
                        }
                        else
                        {
                            v = a[j, k];
                            if ((double)(v) != (double)(0))
                            {
                                i1_ = (0) - (blocksize);
                                for (i_ = blocksize; i_ <= blocksize + j - 1; i_++)
                                {
                                    t[j, i_] = t[j, i_] + v * a[i_ + i1_, k];
                                }
                            }
                        }
                    }
                }

                //
                // Prepare Y (stored in TmpA) and T (stored in TmpT)
                //
                for (k = 0; k <= blocksize - 1; k++)
                {

                    //
                    // fill non-zero part of T, use pre-calculated Gram matrix
                    //
                    i1_ = (blocksize) - (0);
                    for (i_ = 0; i_ <= k - 1; i_++)
                    {
                        work[i_] = t[k, i_ + i1_];
                    }
                    for (i = 0; i <= k - 1; i++)
                    {
                        v = 0.0;
                        for (i_ = i; i_ <= k - 1; i_++)
                        {
                            v += t[i, i_] * work[i_];
                        }
                        t[i, k] = -(tau[k] * v);
                    }
                    t[k, k] = -tau[k];

                    //
                    // Rest of T is filled by zeros
                    //
                    for (i = k + 1; i <= blocksize - 1; i++)
                    {
                        t[i, k] = 0;
                    }
                }
            }


            /*************************************************************************
            Generate block reflector (complex):
            * fill unused parts of reflectors matrix by zeros
            * fill diagonal of reflectors matrix by ones
            * generate triangular factor T


              -- ALGLIB routine --
                 17.02.2010
                 Bochkanov Sergey
            *************************************************************************/
            private static void cmatrixblockreflector(ref complex[,] a,
                ref complex[] tau,
                bool columnwisea,
                int lengtha,
                int blocksize,
                ref complex[,] t,
                ref complex[] work)
            {
                int i = 0;
                int k = 0;
                complex v = 0;
                int i_ = 0;


                //
                // Prepare Y (stored in TmpA) and T (stored in TmpT)
                //
                for (k = 0; k <= blocksize - 1; k++)
                {

                    //
                    // fill beginning of new column with zeros,
                    // load 1.0 in the first non-zero element
                    //
                    if (columnwisea)
                    {
                        for (i = 0; i <= k - 1; i++)
                        {
                            a[i, k] = 0;
                        }
                    }
                    else
                    {
                        for (i = 0; i <= k - 1; i++)
                        {
                            a[k, i] = 0;
                        }
                    }
                    a[k, k] = 1;

                    //
                    // fill non-zero part of T,
                    //
                    for (i = 0; i <= k - 1; i++)
                    {
                        if (columnwisea)
                        {
                            v = 0.0;
                            for (i_ = k; i_ <= lengtha - 1; i_++)
                            {
                                v += math.conj(a[i_, i]) * a[i_, k];
                            }
                        }
                        else
                        {
                            v = 0.0;
                            for (i_ = k; i_ <= lengtha - 1; i_++)
                            {
                                v += a[i, i_] * math.conj(a[k, i_]);
                            }
                        }
                        work[i] = v;
                    }
                    for (i = 0; i <= k - 1; i++)
                    {
                        v = 0.0;
                        for (i_ = i; i_ <= k - 1; i_++)
                        {
                            v += t[i, i_] * work[i_];
                        }
                        t[i, k] = -(tau[k] * v);
                    }
                    t[k, k] = -tau[k];

                    //
                    // Rest of T is filled by zeros
                    //
                    for (i = k + 1; i <= blocksize - 1; i++)
                    {
                        t[i, k] = 0;
                    }
                }
            }
        }
        public class reflections
        {
            /*************************************************************************
            Generation of an elementary reflection transformation

            The subroutine generates elementary reflection H of order N, so that, for
            a given X, the following equality holds true:

                ( X(1) )   ( Beta )
            H * (  ..  ) = (  0   )
                ( X(n) )   (  0   )

            where
                          ( V(1) )
            H = 1 - Tau * (  ..  ) * ( V(1), ..., V(n) )
                          ( V(n) )

            where the first component of vector V equals 1.

            Input parameters:
                X   -   vector. Array whose index ranges within [1..N].
                N   -   reflection order.

            Output parameters:
                X   -   components from 2 to N are replaced with vector V.
                        The first component is replaced with parameter Beta.
                Tau -   scalar value Tau. If X is a null vector, Tau equals 0,
                        otherwise 1 <= Tau <= 2.

            This subroutine is the modification of the DLARFG subroutines from
            the LAPACK library.

            MODIFICATIONS:
                24.12.2005 sign(Alpha) was replaced with an analogous to the Fortran SIGN code.

              -- LAPACK auxiliary routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994
            *************************************************************************/
            public static void generatereflection(ref double[] x,
                int n,
                ref double tau)
            {
                int j = 0;
                double alpha = 0;
                double xnorm = 0;
                double v = 0;
                double beta = 0;
                double mx = 0;
                double s = 0;
                int i_ = 0;

                tau = 0;

                if (n <= 1)
                {
                    tau = 0;
                    return;
                }

                //
                // Scale if needed (to avoid overflow/underflow during intermediate
                // calculations).
                //
                mx = 0;
                for (j = 1; j <= n; j++)
                {
                    mx = Math.Max(Math.Abs(x[j]), mx);
                }
                s = 1;
                if ((double)(mx) != (double)(0))
                {
                    if ((double)(mx) <= (double)(math.minrealnumber / math.machineepsilon))
                    {
                        s = math.minrealnumber / math.machineepsilon;
                        v = 1 / s;
                        for (i_ = 1; i_ <= n; i_++)
                        {
                            x[i_] = v * x[i_];
                        }
                        mx = mx * v;
                    }
                    else
                    {
                        if ((double)(mx) >= (double)(math.maxrealnumber * math.machineepsilon))
                        {
                            s = math.maxrealnumber * math.machineepsilon;
                            v = 1 / s;
                            for (i_ = 1; i_ <= n; i_++)
                            {
                                x[i_] = v * x[i_];
                            }
                            mx = mx * v;
                        }
                    }
                }

                //
                // XNORM = DNRM2( N-1, X, INCX )
                //
                alpha = x[1];
                xnorm = 0;
                if ((double)(mx) != (double)(0))
                {
                    for (j = 2; j <= n; j++)
                    {
                        xnorm = xnorm + math.sqr(x[j] / mx);
                    }
                    xnorm = Math.Sqrt(xnorm) * mx;
                }
                if ((double)(xnorm) == (double)(0))
                {

                    //
                    // H  =  I
                    //
                    tau = 0;
                    x[1] = x[1] * s;
                    return;
                }

                //
                // general case
                //
                mx = Math.Max(Math.Abs(alpha), Math.Abs(xnorm));
                beta = -(mx * Math.Sqrt(math.sqr(alpha / mx) + math.sqr(xnorm / mx)));
                if ((double)(alpha) < (double)(0))
                {
                    beta = -beta;
                }
                tau = (beta - alpha) / beta;
                v = 1 / (alpha - beta);
                for (i_ = 2; i_ <= n; i_++)
                {
                    x[i_] = v * x[i_];
                }
                x[1] = beta;

                //
                // Scale back outputs
                //
                x[1] = x[1] * s;
            }


            /*************************************************************************
            Application of an elementary reflection to a rectangular matrix of size MxN

            The algorithm pre-multiplies the matrix by an elementary reflection transformation
            which is given by column V and scalar Tau (see the description of the
            GenerateReflection procedure). Not the whole matrix but only a part of it
            is transformed (rows from M1 to M2, columns from N1 to N2). Only the elements
            of this submatrix are changed.

            Input parameters:
                C       -   matrix to be transformed.
                Tau     -   scalar defining the transformation.
                V       -   column defining the transformation.
                            Array whose index ranges within [1..M2-M1+1].
                M1, M2  -   range of rows to be transformed.
                N1, N2  -   range of columns to be transformed.
                WORK    -   working array whose indexes goes from N1 to N2.

            Output parameters:
                C       -   the result of multiplying the input matrix C by the
                            transformation matrix which is given by Tau and V.
                            If N1>N2 or M1>M2, C is not modified.

              -- LAPACK auxiliary routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994
            *************************************************************************/
            public static void applyreflectionfromtheleft(ref double[,] c,
                double tau,
                double[] v,
                int m1,
                int m2,
                int n1,
                int n2,
                ref double[] work)
            {
                double t = 0;
                int i = 0;
                int vm = 0;
                int i_ = 0;

                if (((double)(tau) == (double)(0) | n1 > n2) | m1 > m2)
                {
                    return;
                }

                //
                // w := C' * v
                //
                vm = m2 - m1 + 1;
                for (i = n1; i <= n2; i++)
                {
                    work[i] = 0;
                }
                for (i = m1; i <= m2; i++)
                {
                    t = v[i + 1 - m1];
                    for (i_ = n1; i_ <= n2; i_++)
                    {
                        work[i_] = work[i_] + t * c[i, i_];
                    }
                }

                //
                // C := C - tau * v * w'
                //
                for (i = m1; i <= m2; i++)
                {
                    t = v[i - m1 + 1] * tau;
                    for (i_ = n1; i_ <= n2; i_++)
                    {
                        c[i, i_] = c[i, i_] - t * work[i_];
                    }
                }
            }


            /*************************************************************************
            Application of an elementary reflection to a rectangular matrix of size MxN

            The algorithm post-multiplies the matrix by an elementary reflection transformation
            which is given by column V and scalar Tau (see the description of the
            GenerateReflection procedure). Not the whole matrix but only a part of it
            is transformed (rows from M1 to M2, columns from N1 to N2). Only the
            elements of this submatrix are changed.

            Input parameters:
                C       -   matrix to be transformed.
                Tau     -   scalar defining the transformation.
                V       -   column defining the transformation.
                            Array whose index ranges within [1..N2-N1+1].
                M1, M2  -   range of rows to be transformed.
                N1, N2  -   range of columns to be transformed.
                WORK    -   working array whose indexes goes from M1 to M2.

            Output parameters:
                C       -   the result of multiplying the input matrix C by the
                            transformation matrix which is given by Tau and V.
                            If N1>N2 or M1>M2, C is not modified.

              -- LAPACK auxiliary routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994
            *************************************************************************/
            public static void applyreflectionfromtheright(ref double[,] c,
                double tau,
                double[] v,
                int m1,
                int m2,
                int n1,
                int n2,
                ref double[] work)
            {
                double t = 0;
                int i = 0;
                int vm = 0;
                int i_ = 0;
                int i1_ = 0;

                if (((double)(tau) == (double)(0) | n1 > n2) | m1 > m2)
                {
                    return;
                }
                vm = n2 - n1 + 1;
                for (i = m1; i <= m2; i++)
                {
                    i1_ = (1) - (n1);
                    t = 0.0;
                    for (i_ = n1; i_ <= n2; i_++)
                    {
                        t += c[i, i_] * v[i_ + i1_];
                    }
                    t = t * tau;
                    i1_ = (1) - (n1);
                    for (i_ = n1; i_ <= n2; i_++)
                    {
                        c[i, i_] = c[i, i_] - t * v[i_ + i1_];
                    }
                }
            }


        }
        public class creflections
        {
            /*************************************************************************
            Generation of an elementary complex reflection transformation

            The subroutine generates elementary complex reflection H of  order  N,  so
            that, for a given X, the following equality holds true:

                 ( X(1) )   ( Beta )
            H' * (  ..  ) = (  0   ),   H'*H = I,   Beta is a real number
                 ( X(n) )   (  0   )

            where

                          ( V(1) )
            H = 1 - Tau * (  ..  ) * ( conj(V(1)), ..., conj(V(n)) )
                          ( V(n) )

            where the first component of vector V equals 1.

            Input parameters:
                X   -   vector. Array with elements [1..N].
                N   -   reflection order.

            Output parameters:
                X   -   components from 2 to N are replaced by vector V.
                        The first component is replaced with parameter Beta.
                Tau -   scalar value Tau.

            This subroutine is the modification of CLARFG subroutines  from the LAPACK
            library. It has similar functionality except for the fact that it  doesn’t
            handle errors when intermediate results cause an overflow.

              -- LAPACK auxiliary routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994
            *************************************************************************/
            public static void complexgeneratereflection(ref complex[] x,
                int n,
                ref complex tau)
            {
                int j = 0;
                complex alpha = 0;
                double alphi = 0;
                double alphr = 0;
                double beta = 0;
                double xnorm = 0;
                double mx = 0;
                complex t = 0;
                double s = 0;
                complex v = 0;
                int i_ = 0;

                tau = 0;

                if (n <= 0)
                {
                    tau = 0;
                    return;
                }

                //
                // Scale if needed (to avoid overflow/underflow during intermediate
                // calculations).
                //
                mx = 0;
                for (j = 1; j <= n; j++)
                {
                    mx = Math.Max(math.abscomplex(x[j]), mx);
                }
                s = 1;
                if ((double)(mx) != (double)(0))
                {
                    if ((double)(mx) < (double)(1))
                    {
                        s = Math.Sqrt(math.minrealnumber);
                        v = 1 / s;
                        for (i_ = 1; i_ <= n; i_++)
                        {
                            x[i_] = v * x[i_];
                        }
                    }
                    else
                    {
                        s = Math.Sqrt(math.maxrealnumber);
                        v = 1 / s;
                        for (i_ = 1; i_ <= n; i_++)
                        {
                            x[i_] = v * x[i_];
                        }
                    }
                }

                //
                // calculate
                //
                alpha = x[1];
                mx = 0;
                for (j = 2; j <= n; j++)
                {
                    mx = Math.Max(math.abscomplex(x[j]), mx);
                }
                xnorm = 0;
                if ((double)(mx) != (double)(0))
                {
                    for (j = 2; j <= n; j++)
                    {
                        t = x[j] / mx;
                        xnorm = xnorm + (t * math.conj(t)).x;
                    }
                    xnorm = Math.Sqrt(xnorm) * mx;
                }
                alphr = alpha.x;
                alphi = alpha.y;
                if ((double)(xnorm) == (double)(0) & (double)(alphi) == (double)(0))
                {
                    tau = 0;
                    x[1] = x[1] * s;
                    return;
                }
                mx = Math.Max(Math.Abs(alphr), Math.Abs(alphi));
                mx = Math.Max(mx, Math.Abs(xnorm));
                beta = -(mx * Math.Sqrt(math.sqr(alphr / mx) + math.sqr(alphi / mx) + math.sqr(xnorm / mx)));
                if ((double)(alphr) < (double)(0))
                {
                    beta = -beta;
                }
                tau.x = (beta - alphr) / beta;
                tau.y = -(alphi / beta);
                alpha = 1 / (alpha - beta);
                if (n > 1)
                {
                    for (i_ = 2; i_ <= n; i_++)
                    {
                        x[i_] = alpha * x[i_];
                    }
                }
                alpha = beta;
                x[1] = alpha;

                //
                // Scale back
                //
                x[1] = x[1] * s;
            }


            /*************************************************************************
            Application of an elementary reflection to a rectangular matrix of size MxN

            The  algorithm  pre-multiplies  the  matrix  by  an  elementary reflection
            transformation  which  is  given  by  column  V  and  scalar  Tau (see the
            description of the GenerateReflection). Not the whole matrix  but  only  a
            part of it is transformed (rows from M1 to M2, columns from N1 to N2). Only
            the elements of this submatrix are changed.

            Note: the matrix is multiplied by H, not by H'.   If  it  is  required  to
            multiply the matrix by H', it is necessary to pass Conj(Tau) instead of Tau.

            Input parameters:
                C       -   matrix to be transformed.
                Tau     -   scalar defining transformation.
                V       -   column defining transformation.
                            Array whose index ranges within [1..M2-M1+1]
                M1, M2  -   range of rows to be transformed.
                N1, N2  -   range of columns to be transformed.
                WORK    -   working array whose index goes from N1 to N2.

            Output parameters:
                C       -   the result of multiplying the input matrix C by the
                            transformation matrix which is given by Tau and V.
                            If N1>N2 or M1>M2, C is not modified.

              -- LAPACK auxiliary routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994
            *************************************************************************/
            public static void complexapplyreflectionfromtheleft(ref complex[,] c,
                complex tau,
                complex[] v,
                int m1,
                int m2,
                int n1,
                int n2,
                ref complex[] work)
            {
                complex t = 0;
                int i = 0;
                int vm = 0;
                int i_ = 0;

                if ((tau == 0 | n1 > n2) | m1 > m2)
                {
                    return;
                }

                //
                // w := C^T * conj(v)
                //
                vm = m2 - m1 + 1;
                for (i = n1; i <= n2; i++)
                {
                    work[i] = 0;
                }
                for (i = m1; i <= m2; i++)
                {
                    t = math.conj(v[i + 1 - m1]);
                    for (i_ = n1; i_ <= n2; i_++)
                    {
                        work[i_] = work[i_] + t * c[i, i_];
                    }
                }

                //
                // C := C - tau * v * w^T
                //
                for (i = m1; i <= m2; i++)
                {
                    t = v[i - m1 + 1] * tau;
                    for (i_ = n1; i_ <= n2; i_++)
                    {
                        c[i, i_] = c[i, i_] - t * work[i_];
                    }
                }
            }


            /*************************************************************************
            Application of an elementary reflection to a rectangular matrix of size MxN

            The  algorithm  post-multiplies  the  matrix  by  an elementary reflection
            transformation  which  is  given  by  column  V  and  scalar  Tau (see the
            description  of  the  GenerateReflection). Not the whole matrix but only a
            part  of  it  is  transformed (rows from M1 to M2, columns from N1 to N2).
            Only the elements of this submatrix are changed.

            Input parameters:
                C       -   matrix to be transformed.
                Tau     -   scalar defining transformation.
                V       -   column defining transformation.
                            Array whose index ranges within [1..N2-N1+1]
                M1, M2  -   range of rows to be transformed.
                N1, N2  -   range of columns to be transformed.
                WORK    -   working array whose index goes from M1 to M2.

            Output parameters:
                C       -   the result of multiplying the input matrix C by the
                            transformation matrix which is given by Tau and V.
                            If N1>N2 or M1>M2, C is not modified.

              -- LAPACK auxiliary routine (version 3.0) --
                 Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
                 Courant Institute, Argonne National Lab, and Rice University
                 September 30, 1994
            *************************************************************************/
            public static void complexapplyreflectionfromtheright(ref complex[,] c,
                complex tau,
                ref complex[] v,
                int m1,
                int m2,
                int n1,
                int n2,
                ref complex[] work)
            {
                complex t = 0;
                int i = 0;
                int vm = 0;
                int i_ = 0;
                int i1_ = 0;

                if ((tau == 0 | n1 > n2) | m1 > m2)
                {
                    return;
                }

                //
                // w := C * v
                //
                vm = n2 - n1 + 1;
                for (i = m1; i <= m2; i++)
                {
                    i1_ = (1) - (n1);
                    t = 0.0;
                    for (i_ = n1; i_ <= n2; i_++)
                    {
                        t += c[i, i_] * v[i_ + i1_];
                    }
                    work[i] = t;
                }

                //
                // C := C - w * conj(v^T)
                //
                for (i_ = 1; i_ <= vm; i_++)
                {
                    v[i_] = math.conj(v[i_]);
                }
                for (i = m1; i <= m2; i++)
                {
                    t = work[i] * tau;
                    i1_ = (1) - (n1);
                    for (i_ = n1; i_ <= n2; i_++)
                    {
                        c[i, i_] = c[i, i_] - t * v[i_ + i1_];
                    }
                }
                for (i_ = 1; i_ <= vm; i_++)
                {
                    v[i_] = math.conj(v[i_]);
                }
            }


        }
        public class hblas
        {
            public static void hermitianmatrixvectormultiply(complex[,] a,
                bool isupper,
                int i1,
                int i2,
                complex[] x,
                complex alpha,
                ref complex[] y)
            {
                int i = 0;
                int ba1 = 0;
                int ba2 = 0;
                int by1 = 0;
                int by2 = 0;
                int bx1 = 0;
                int bx2 = 0;
                int n = 0;
                complex v = 0;
                int i_ = 0;
                int i1_ = 0;

                n = i2 - i1 + 1;
                if (n <= 0)
                {
                    return;
                }

                //
                // Let A = L + D + U, where
                //  L is strictly lower triangular (main diagonal is zero)
                //  D is diagonal
                //  U is strictly upper triangular (main diagonal is zero)
                //
                // A*x = L*x + D*x + U*x
                //
                // Calculate D*x first
                //
                for (i = i1; i <= i2; i++)
                {
                    y[i - i1 + 1] = a[i, i] * x[i - i1 + 1];
                }

                //
                // Add L*x + U*x
                //
                if (isupper)
                {
                    for (i = i1; i <= i2 - 1; i++)
                    {

                        //
                        // Add L*x to the result
                        //
                        v = x[i - i1 + 1];
                        by1 = i - i1 + 2;
                        by2 = n;
                        ba1 = i + 1;
                        ba2 = i2;
                        i1_ = (ba1) - (by1);
                        for (i_ = by1; i_ <= by2; i_++)
                        {
                            y[i_] = y[i_] + v * math.conj(a[i, i_ + i1_]);
                        }

                        //
                        // Add U*x to the result
                        //
                        bx1 = i - i1 + 2;
                        bx2 = n;
                        ba1 = i + 1;
                        ba2 = i2;
                        i1_ = (ba1) - (bx1);
                        v = 0.0;
                        for (i_ = bx1; i_ <= bx2; i_++)
                        {
                            v += x[i_] * a[i, i_ + i1_];
                        }
                        y[i - i1 + 1] = y[i - i1 + 1] + v;
                    }
                }
                else
                {
                    for (i = i1 + 1; i <= i2; i++)
                    {

                        //
                        // Add L*x to the result
                        //
                        bx1 = 1;
                        bx2 = i - i1;
                        ba1 = i1;
                        ba2 = i - 1;
                        i1_ = (ba1) - (bx1);
                        v = 0.0;
                        for (i_ = bx1; i_ <= bx2; i_++)
                        {
                            v += x[i_] * a[i, i_ + i1_];
                        }
                        y[i - i1 + 1] = y[i - i1 + 1] + v;

                        //
                        // Add U*x to the result
                        //
                        v = x[i - i1 + 1];
                        by1 = 1;
                        by2 = i - i1;
                        ba1 = i1;
                        ba2 = i - 1;
                        i1_ = (ba1) - (by1);
                        for (i_ = by1; i_ <= by2; i_++)
                        {
                            y[i_] = y[i_] + v * math.conj(a[i, i_ + i1_]);
                        }
                    }
                }
                for (i_ = 1; i_ <= n; i_++)
                {
                    y[i_] = alpha * y[i_];
                }
            }


            public static void hermitianrank2update(ref complex[,] a,
                bool isupper,
                int i1,
                int i2,
                complex[] x,
                complex[] y,
                ref complex[] t,
                complex alpha)
            {
                int i = 0;
                int tp1 = 0;
                int tp2 = 0;
                complex v = 0;
                int i_ = 0;
                int i1_ = 0;

                if (isupper)
                {
                    for (i = i1; i <= i2; i++)
                    {
                        tp1 = i + 1 - i1;
                        tp2 = i2 - i1 + 1;
                        v = alpha * x[i + 1 - i1];
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = v * math.conj(y[i_]);
                        }
                        v = math.conj(alpha) * y[i + 1 - i1];
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = t[i_] + v * math.conj(x[i_]);
                        }
                        i1_ = (tp1) - (i);
                        for (i_ = i; i_ <= i2; i_++)
                        {
                            a[i, i_] = a[i, i_] + t[i_ + i1_];
                        }
                    }
                }
                else
                {
                    for (i = i1; i <= i2; i++)
                    {
                        tp1 = 1;
                        tp2 = i + 1 - i1;
                        v = alpha * x[i + 1 - i1];
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = v * math.conj(y[i_]);
                        }
                        v = math.conj(alpha) * y[i + 1 - i1];
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = t[i_] + v * math.conj(x[i_]);
                        }
                        i1_ = (tp1) - (i1);
                        for (i_ = i1; i_ <= i; i_++)
                        {
                            a[i, i_] = a[i, i_] + t[i_ + i1_];
                        }
                    }
                }
            }


        }
        public class rotations
        {
            /*************************************************************************
            Application of a sequence of  elementary rotations to a matrix

            The algorithm pre-multiplies the matrix by a sequence of rotation
            transformations which is given by arrays C and S. Depending on the value
            of the IsForward parameter either 1 and 2, 3 and 4 and so on (if IsForward=true)
            rows are rotated, or the rows N and N-1, N-2 and N-3 and so on, are rotated.

            Not the whole matrix but only a part of it is transformed (rows from M1 to
            M2, columns from N1 to N2). Only the elements of this submatrix are changed.

            Input parameters:
                IsForward   -   the sequence of the rotation application.
                M1,M2       -   the range of rows to be transformed.
                N1, N2      -   the range of columns to be transformed.
                C,S         -   transformation coefficients.
                                Array whose index ranges within [1..M2-M1].
                A           -   processed matrix.
                WORK        -   working array whose index ranges within [N1..N2].

            Output parameters:
                A           -   transformed matrix.

            Utility subroutine.
            *************************************************************************/
            public static void applyrotationsfromtheleft(bool isforward,
                int m1,
                int m2,
                int n1,
                int n2,
                double[] c,
                double[] s,
                ref double[,] a,
                ref double[] work)
            {
                int j = 0;
                int jp1 = 0;
                double ctemp = 0;
                double stemp = 0;
                double temp = 0;
                int i_ = 0;

                if (m1 > m2 | n1 > n2)
                {
                    return;
                }

                //
                // Form  P * A
                //
                if (isforward)
                {
                    if (n1 != n2)
                    {

                        //
                        // Common case: N1<>N2
                        //
                        for (j = m1; j <= m2 - 1; j++)
                        {
                            ctemp = c[j - m1 + 1];
                            stemp = s[j - m1 + 1];
                            if ((double)(ctemp) != (double)(1) | (double)(stemp) != (double)(0))
                            {
                                jp1 = j + 1;
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    work[i_] = ctemp * a[jp1, i_];
                                }
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    work[i_] = work[i_] - stemp * a[j, i_];
                                }
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    a[j, i_] = ctemp * a[j, i_];
                                }
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    a[j, i_] = a[j, i_] + stemp * a[jp1, i_];
                                }
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    a[jp1, i_] = work[i_];
                                }
                            }
                        }
                    }
                    else
                    {

                        //
                        // Special case: N1=N2
                        //
                        for (j = m1; j <= m2 - 1; j++)
                        {
                            ctemp = c[j - m1 + 1];
                            stemp = s[j - m1 + 1];
                            if ((double)(ctemp) != (double)(1) | (double)(stemp) != (double)(0))
                            {
                                temp = a[j + 1, n1];
                                a[j + 1, n1] = ctemp * temp - stemp * a[j, n1];
                                a[j, n1] = stemp * temp + ctemp * a[j, n1];
                            }
                        }
                    }
                }
                else
                {
                    if (n1 != n2)
                    {

                        //
                        // Common case: N1<>N2
                        //
                        for (j = m2 - 1; j >= m1; j--)
                        {
                            ctemp = c[j - m1 + 1];
                            stemp = s[j - m1 + 1];
                            if ((double)(ctemp) != (double)(1) | (double)(stemp) != (double)(0))
                            {
                                jp1 = j + 1;
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    work[i_] = ctemp * a[jp1, i_];
                                }
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    work[i_] = work[i_] - stemp * a[j, i_];
                                }
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    a[j, i_] = ctemp * a[j, i_];
                                }
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    a[j, i_] = a[j, i_] + stemp * a[jp1, i_];
                                }
                                for (i_ = n1; i_ <= n2; i_++)
                                {
                                    a[jp1, i_] = work[i_];
                                }
                            }
                        }
                    }
                    else
                    {

                        //
                        // Special case: N1=N2
                        //
                        for (j = m2 - 1; j >= m1; j--)
                        {
                            ctemp = c[j - m1 + 1];
                            stemp = s[j - m1 + 1];
                            if ((double)(ctemp) != (double)(1) | (double)(stemp) != (double)(0))
                            {
                                temp = a[j + 1, n1];
                                a[j + 1, n1] = ctemp * temp - stemp * a[j, n1];
                                a[j, n1] = stemp * temp + ctemp * a[j, n1];
                            }
                        }
                    }
                }
            }


            /*************************************************************************
            Application of a sequence of  elementary rotations to a matrix

            The algorithm post-multiplies the matrix by a sequence of rotation
            transformations which is given by arrays C and S. Depending on the value
            of the IsForward parameter either 1 and 2, 3 and 4 and so on (if IsForward=true)
            rows are rotated, or the rows N and N-1, N-2 and N-3 and so on are rotated.

            Not the whole matrix but only a part of it is transformed (rows from M1
            to M2, columns from N1 to N2). Only the elements of this submatrix are changed.

            Input parameters:
                IsForward   -   the sequence of the rotation application.
                M1,M2       -   the range of rows to be transformed.
                N1, N2      -   the range of columns to be transformed.
                C,S         -   transformation coefficients.
                                Array whose index ranges within [1..N2-N1].
                A           -   processed matrix.
                WORK        -   working array whose index ranges within [M1..M2].

            Output parameters:
                A           -   transformed matrix.

            Utility subroutine.
            *************************************************************************/
            public static void applyrotationsfromtheright(bool isforward,
                int m1,
                int m2,
                int n1,
                int n2,
                double[] c,
                double[] s,
                ref double[,] a,
                ref double[] work)
            {
                int j = 0;
                int jp1 = 0;
                double ctemp = 0;
                double stemp = 0;
                double temp = 0;
                int i_ = 0;


                //
                // Form A * P'
                //
                if (isforward)
                {
                    if (m1 != m2)
                    {

                        //
                        // Common case: M1<>M2
                        //
                        for (j = n1; j <= n2 - 1; j++)
                        {
                            ctemp = c[j - n1 + 1];
                            stemp = s[j - n1 + 1];
                            if ((double)(ctemp) != (double)(1) | (double)(stemp) != (double)(0))
                            {
                                jp1 = j + 1;
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    work[i_] = ctemp * a[i_, jp1];
                                }
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    work[i_] = work[i_] - stemp * a[i_, j];
                                }
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    a[i_, j] = ctemp * a[i_, j];
                                }
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    a[i_, j] = a[i_, j] + stemp * a[i_, jp1];
                                }
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    a[i_, jp1] = work[i_];
                                }
                            }
                        }
                    }
                    else
                    {

                        //
                        // Special case: M1=M2
                        //
                        for (j = n1; j <= n2 - 1; j++)
                        {
                            ctemp = c[j - n1 + 1];
                            stemp = s[j - n1 + 1];
                            if ((double)(ctemp) != (double)(1) | (double)(stemp) != (double)(0))
                            {
                                temp = a[m1, j + 1];
                                a[m1, j + 1] = ctemp * temp - stemp * a[m1, j];
                                a[m1, j] = stemp * temp + ctemp * a[m1, j];
                            }
                        }
                    }
                }
                else
                {
                    if (m1 != m2)
                    {

                        //
                        // Common case: M1<>M2
                        //
                        for (j = n2 - 1; j >= n1; j--)
                        {
                            ctemp = c[j - n1 + 1];
                            stemp = s[j - n1 + 1];
                            if ((double)(ctemp) != (double)(1) | (double)(stemp) != (double)(0))
                            {
                                jp1 = j + 1;
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    work[i_] = ctemp * a[i_, jp1];
                                }
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    work[i_] = work[i_] - stemp * a[i_, j];
                                }
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    a[i_, j] = ctemp * a[i_, j];
                                }
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    a[i_, j] = a[i_, j] + stemp * a[i_, jp1];
                                }
                                for (i_ = m1; i_ <= m2; i_++)
                                {
                                    a[i_, jp1] = work[i_];
                                }
                            }
                        }
                    }
                    else
                    {

                        //
                        // Special case: M1=M2
                        //
                        for (j = n2 - 1; j >= n1; j--)
                        {
                            ctemp = c[j - n1 + 1];
                            stemp = s[j - n1 + 1];
                            if ((double)(ctemp) != (double)(1) | (double)(stemp) != (double)(0))
                            {
                                temp = a[m1, j + 1];
                                a[m1, j + 1] = ctemp * temp - stemp * a[m1, j];
                                a[m1, j] = stemp * temp + ctemp * a[m1, j];
                            }
                        }
                    }
                }
            }


            /*************************************************************************
            The subroutine generates the elementary rotation, so that:

            [  CS  SN  ]  .  [ F ]  =  [ R ]
            [ -SN  CS  ]     [ G ]     [ 0 ]

            CS**2 + SN**2 = 1
            *************************************************************************/
            public static void generaterotation(double f,
                double g,
                ref double cs,
                ref double sn,
                ref double r)
            {
                double f1 = 0;
                double g1 = 0;

                cs = 0;
                sn = 0;
                r = 0;

                if ((double)(g) == (double)(0))
                {
                    cs = 1;
                    sn = 0;
                    r = f;
                }
                else
                {
                    if ((double)(f) == (double)(0))
                    {
                        cs = 0;
                        sn = 1;
                        r = g;
                    }
                    else
                    {
                        f1 = f;
                        g1 = g;
                        if ((double)(Math.Abs(f1)) > (double)(Math.Abs(g1)))
                        {
                            r = Math.Abs(f1) * Math.Sqrt(1 + math.sqr(g1 / f1));
                        }
                        else
                        {
                            r = Math.Abs(g1) * Math.Sqrt(1 + math.sqr(f1 / g1));
                        }
                        cs = f1 / r;
                        sn = g1 / r;
                        if ((double)(Math.Abs(f)) > (double)(Math.Abs(g)) & (double)(cs) < (double)(0))
                        {
                            cs = -cs;
                            sn = -sn;
                            r = -r;
                        }
                    }
                }
            }


        }
        public class sblas
        {
            public static void symmetricmatrixvectormultiply(double[,] a,
                bool isupper,
                int i1,
                int i2,
                double[] x,
                double alpha,
                ref double[] y)
            {
                int i = 0;
                int ba1 = 0;
                int ba2 = 0;
                int by1 = 0;
                int by2 = 0;
                int bx1 = 0;
                int bx2 = 0;
                int n = 0;
                double v = 0;
                int i_ = 0;
                int i1_ = 0;

                n = i2 - i1 + 1;
                if (n <= 0)
                {
                    return;
                }

                //
                // Let A = L + D + U, where
                //  L is strictly lower triangular (main diagonal is zero)
                //  D is diagonal
                //  U is strictly upper triangular (main diagonal is zero)
                //
                // A*x = L*x + D*x + U*x
                //
                // Calculate D*x first
                //
                for (i = i1; i <= i2; i++)
                {
                    y[i - i1 + 1] = a[i, i] * x[i - i1 + 1];
                }

                //
                // Add L*x + U*x
                //
                if (isupper)
                {
                    for (i = i1; i <= i2 - 1; i++)
                    {

                        //
                        // Add L*x to the result
                        //
                        v = x[i - i1 + 1];
                        by1 = i - i1 + 2;
                        by2 = n;
                        ba1 = i + 1;
                        ba2 = i2;
                        i1_ = (ba1) - (by1);
                        for (i_ = by1; i_ <= by2; i_++)
                        {
                            y[i_] = y[i_] + v * a[i, i_ + i1_];
                        }

                        //
                        // Add U*x to the result
                        //
                        bx1 = i - i1 + 2;
                        bx2 = n;
                        ba1 = i + 1;
                        ba2 = i2;
                        i1_ = (ba1) - (bx1);
                        v = 0.0;
                        for (i_ = bx1; i_ <= bx2; i_++)
                        {
                            v += x[i_] * a[i, i_ + i1_];
                        }
                        y[i - i1 + 1] = y[i - i1 + 1] + v;
                    }
                }
                else
                {
                    for (i = i1 + 1; i <= i2; i++)
                    {

                        //
                        // Add L*x to the result
                        //
                        bx1 = 1;
                        bx2 = i - i1;
                        ba1 = i1;
                        ba2 = i - 1;
                        i1_ = (ba1) - (bx1);
                        v = 0.0;
                        for (i_ = bx1; i_ <= bx2; i_++)
                        {
                            v += x[i_] * a[i, i_ + i1_];
                        }
                        y[i - i1 + 1] = y[i - i1 + 1] + v;

                        //
                        // Add U*x to the result
                        //
                        v = x[i - i1 + 1];
                        by1 = 1;
                        by2 = i - i1;
                        ba1 = i1;
                        ba2 = i - 1;
                        i1_ = (ba1) - (by1);
                        for (i_ = by1; i_ <= by2; i_++)
                        {
                            y[i_] = y[i_] + v * a[i, i_ + i1_];
                        }
                    }
                }
                for (i_ = 1; i_ <= n; i_++)
                {
                    y[i_] = alpha * y[i_];
                }
            }


            public static void symmetricrank2update(ref double[,] a,
                bool isupper,
                int i1,
                int i2,
                double[] x,
                double[] y,
                ref double[] t,
                double alpha)
            {
                int i = 0;
                int tp1 = 0;
                int tp2 = 0;
                double v = 0;
                int i_ = 0;
                int i1_ = 0;

                if (isupper)
                {
                    for (i = i1; i <= i2; i++)
                    {
                        tp1 = i + 1 - i1;
                        tp2 = i2 - i1 + 1;
                        v = x[i + 1 - i1];
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = v * y[i_];
                        }
                        v = y[i + 1 - i1];
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = t[i_] + v * x[i_];
                        }
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = alpha * t[i_];
                        }
                        i1_ = (tp1) - (i);
                        for (i_ = i; i_ <= i2; i_++)
                        {
                            a[i, i_] = a[i, i_] + t[i_ + i1_];
                        }
                    }
                }
                else
                {
                    for (i = i1; i <= i2; i++)
                    {
                        tp1 = 1;
                        tp2 = i + 1 - i1;
                        v = x[i + 1 - i1];
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = v * y[i_];
                        }
                        v = y[i + 1 - i1];
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = t[i_] + v * x[i_];
                        }
                        for (i_ = tp1; i_ <= tp2; i_++)
                        {
                            t[i_] = alpha * t[i_];
                        }
                        i1_ = (tp1) - (i1);
                        for (i_ = i1; i_ <= i; i_++)
                        {
                            a[i, i_] = a[i, i_] + t[i_ + i1_];
                        }
                    }
                }
            }


        }
        public class basicstatops
        {
            /*************************************************************************
            Internal ranking subroutine
            *************************************************************************/
            public static void rankx(ref double[] x,
                int n,
                apserv.apbuffers buf)
            {
                int i = 0;
                int j = 0;
                int k = 0;
                int t = 0;
                double tmp = 0;
                int tmpi = 0;


                //
                // Prepare
                //
                if (n < 1)
                {
                    return;
                }
                if (n == 1)
                {
                    x[0] = 1;
                    return;
                }
                if (ap.len(buf.ra1) < n)
                {
                    buf.ra1 = new double[n];
                }
                if (ap.len(buf.ia1) < n)
                {
                    buf.ia1 = new int[n];
                }
                for (i = 0; i <= n - 1; i++)
                {
                    buf.ra1[i] = x[i];
                    buf.ia1[i] = i;
                }

                //
                // sort {R, C}
                //
                if (n != 1)
                {
                    i = 2;
                    do
                    {
                        t = i;
                        while (t != 1)
                        {
                            k = t / 2;
                            if ((double)(buf.ra1[k - 1]) >= (double)(buf.ra1[t - 1]))
                            {
                                t = 1;
                            }
                            else
                            {
                                tmp = buf.ra1[k - 1];
                                buf.ra1[k - 1] = buf.ra1[t - 1];
                                buf.ra1[t - 1] = tmp;
                                tmpi = buf.ia1[k - 1];
                                buf.ia1[k - 1] = buf.ia1[t - 1];
                                buf.ia1[t - 1] = tmpi;
                                t = k;
                            }
                        }
                        i = i + 1;
                    }
                    while (i <= n);
                    i = n - 1;
                    do
                    {
                        tmp = buf.ra1[i];
                        buf.ra1[i] = buf.ra1[0];
                        buf.ra1[0] = tmp;
                        tmpi = buf.ia1[i];
                        buf.ia1[i] = buf.ia1[0];
                        buf.ia1[0] = tmpi;
                        t = 1;
                        while (t != 0)
                        {
                            k = 2 * t;
                            if (k > i)
                            {
                                t = 0;
                            }
                            else
                            {
                                if (k < i)
                                {
                                    if ((double)(buf.ra1[k]) > (double)(buf.ra1[k - 1]))
                                    {
                                        k = k + 1;
                                    }
                                }
                                if ((double)(buf.ra1[t - 1]) >= (double)(buf.ra1[k - 1]))
                                {
                                    t = 0;
                                }
                                else
                                {
                                    tmp = buf.ra1[k - 1];
                                    buf.ra1[k - 1] = buf.ra1[t - 1];
                                    buf.ra1[t - 1] = tmp;
                                    tmpi = buf.ia1[k - 1];
                                    buf.ia1[k - 1] = buf.ia1[t - 1];
                                    buf.ia1[t - 1] = tmpi;
                                    t = k;
                                }
                            }
                        }
                        i = i - 1;
                    }
                    while (i >= 1);
                }

                //
                // compute tied ranks
                //
                i = 0;
                while (i <= n - 1)
                {
                    j = i + 1;
                    while (j <= n - 1)
                    {
                        if ((double)(buf.ra1[j]) != (double)(buf.ra1[i]))
                        {
                            break;
                        }
                        j = j + 1;
                    }
                    for (k = i; k <= j - 1; k++)
                    {
                        buf.ra1[k] = 1 + (double)(i + j - 1) / (double)2;
                    }
                    i = j;
                }

                //
                // back to x
                //
                for (i = 0; i <= n - 1; i++)
                {
                    x[buf.ia1[i]] = buf.ra1[i];
                }
            }


        }
        /********************************************************************
           serializer object (should not be used directly)
           ********************************************************************/
        public class serializer
        {
            enum SMODE { DEFAULT, ALLOC, TO_STRING, FROM_STRING };
            private const int SER_ENTRIES_PER_ROW = 5;
            private const int SER_ENTRY_LENGTH = 11;

            private SMODE mode;
            private int entries_needed;
            private int entries_saved;
            private int bytes_asked;
            private int bytes_written;
            private int bytes_read;
            private char[] out_str;
            private char[] in_str;

            public serializer()
            {
                mode = SMODE.DEFAULT;
                entries_needed = 0;
                bytes_asked = 0;
            }

            public void alloc_start()
            {
                entries_needed = 0;
                bytes_asked = 0;
                mode = SMODE.ALLOC;
            }

            public void alloc_entry()
            {
                if (mode != SMODE.ALLOC)
                {
                    // throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }
                entries_needed++;
            }

            private int get_alloc_size()
            {
                int rows, lastrowsize, result;

                // check and change mode
                if (mode != SMODE.ALLOC)
                {
                    // throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }

                // if no entries needes (degenerate case)
                if (entries_needed == 0)
                {
                    bytes_asked = 1;
                    return bytes_asked;
                }

                // non-degenerate case
                rows = entries_needed / SER_ENTRIES_PER_ROW;
                lastrowsize = SER_ENTRIES_PER_ROW;
                if (entries_needed % SER_ENTRIES_PER_ROW != 0)
                {
                    lastrowsize = entries_needed % SER_ENTRIES_PER_ROW;
                    rows++;
                }

                // calculate result size
                result = ((rows - 1) * SER_ENTRIES_PER_ROW + lastrowsize) * SER_ENTRY_LENGTH;
                result += (rows - 1) * (SER_ENTRIES_PER_ROW - 1) + (lastrowsize - 1);
                result += rows * 2;
                bytes_asked = result;
                return result;
            }

            public void sstart_str()
            {
                int allocsize = get_alloc_size();

                // check and change mode
                if (mode != SMODE.ALLOC)
                {
                    // throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }
                mode = SMODE.TO_STRING;

                // other preparations
                out_str = new char[allocsize];
                entries_saved = 0;
                bytes_written = 0;
            }

            public void ustart_str(string s)
            {
                // check and change mode
                if (mode != SMODE.DEFAULT)
                {
                    //  throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }
                mode = SMODE.FROM_STRING;

                in_str = s.ToCharArray();
                bytes_read = 0;
            }

            public void serialize_bool(bool v)
            {
                if (mode != SMODE.TO_STRING)
                {
                    //   throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }
                bool2str(v, out_str, ref bytes_written);
                entries_saved++;
                if (entries_saved % SER_ENTRIES_PER_ROW != 0)
                {
                    out_str[bytes_written] = ' ';
                    bytes_written++;
                }
                else
                {
                    out_str[bytes_written + 0] = '\r';
                    out_str[bytes_written + 1] = '\n';
                    bytes_written += 2;
                }
            }

            public void serialize_int(int v)
            {
                if (mode != SMODE.TO_STRING)
                {
                    // throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }
                int2str(v, out_str, ref bytes_written);
                entries_saved++;
                if (entries_saved % SER_ENTRIES_PER_ROW != 0)
                {
                    out_str[bytes_written] = ' ';
                    bytes_written++;
                }
                else
                {
                    out_str[bytes_written + 0] = '\r';
                    out_str[bytes_written + 1] = '\n';
                    bytes_written += 2;
                }
            }

            public void serialize_double(double v)
            {
                if (mode != SMODE.TO_STRING)
                {
                    //  throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }
                double2str(v, out_str, ref bytes_written);
                entries_saved++;
                if (entries_saved % SER_ENTRIES_PER_ROW != 0)
                {
                    out_str[bytes_written] = ' ';
                    bytes_written++;
                }
                else
                {
                    out_str[bytes_written + 0] = '\r';
                    out_str[bytes_written + 1] = '\n';
                    bytes_written += 2;
                }
            }

            public bool unserialize_bool()
            {
                if (mode != SMODE.FROM_STRING)
                {
                    //   throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }
                return str2bool(in_str, ref bytes_read);
            }

            public int unserialize_int()
            {
                if (mode != SMODE.FROM_STRING)
                {
                    //   throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }
                return str2int(in_str, ref bytes_read);
            }

            public double unserialize_double()
            {
                if (mode != SMODE.FROM_STRING)
                {
                    //   throw new alglib.alglibexception("ALGLIB: internal error during (un)serialization");
                }
                return str2double(in_str, ref bytes_read);
            }

            public void stop()
            {
            }

            public string get_string()
            {
                return new string(out_str, 0, bytes_written);
            }


            /************************************************************************
            This function converts six-bit value (from 0 to 63)  to  character  (only
            digits, lowercase and uppercase letters, minus and underscore are used).

            If v is negative or greater than 63, this function returns '?'.
            ************************************************************************/
            private static char[] _sixbits2char_tbl = new char[64]{ 
                '0', '1', '2', '3', '4', '5', '6', '7',
                '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
                'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
                'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
                'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 
                'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 
                'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 
                'u', 'v', 'w', 'x', 'y', 'z', '-', '_' };
            private static char sixbits2char(int v)
            {
                if (v < 0 || v > 63)
                    return '?';
                return _sixbits2char_tbl[v];
            }

            /************************************************************************
            This function converts character to six-bit value (from 0 to 63).

            This function is inverse of ae_sixbits2char()
            If c is not correct character, this function returns -1.
            ************************************************************************/
            private static int[] _char2sixbits_tbl = new int[128] {
            -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, 62, -1, -1,
             0,  1,  2,  3,  4,  5,  6,  7,
             8,  9, -1, -1, -1, -1, -1, -1,
            -1, 10, 11, 12, 13, 14, 15, 16,
            17, 18, 19, 20, 21, 22, 23, 24,
            25, 26, 27, 28, 29, 30, 31, 32,
            33, 34, 35, -1, -1, -1, -1, 63,
            -1, 36, 37, 38, 39, 40, 41, 42,
            43, 44, 45, 46, 47, 48, 49, 50,
            51, 52, 53, 54, 55, 56, 57, 58,
            59, 60, 61, -1, -1, -1, -1, -1 };
            private static int char2sixbits(char c)
            {
                return (c >= 0 && c < 127) ? _char2sixbits_tbl[c] : -1;
            }

            /************************************************************************
            This function converts three bytes (24 bits) to four six-bit values 
            (24 bits again).

            src         array
            src_offs    offset of three-bytes chunk
            dst         array for ints
            dst_offs    offset of four-ints chunk
            ************************************************************************/
            private static void threebytes2foursixbits(byte[] src, int src_offs, int[] dst, int dst_offs)
            {
                dst[dst_offs + 0] = src[src_offs + 0] & 0x3F;
                dst[dst_offs + 1] = (src[src_offs + 0] >> 6) | ((src[src_offs + 1] & 0x0F) << 2);
                dst[dst_offs + 2] = (src[src_offs + 1] >> 4) | ((src[src_offs + 2] & 0x03) << 4);
                dst[dst_offs + 3] = src[src_offs + 2] >> 2;
            }

            /************************************************************************
            This function converts four six-bit values (24 bits) to three bytes
            (24 bits again).

            src         pointer to four ints
            src_offs    offset of the chunk
            dst         pointer to three bytes
            dst_offs    offset of the chunk
            ************************************************************************/
            private static void foursixbits2threebytes(int[] src, int src_offs, byte[] dst, int dst_offs)
            {
                dst[dst_offs + 0] = (byte)(src[src_offs + 0] | ((src[src_offs + 1] & 0x03) << 6));
                dst[dst_offs + 1] = (byte)((src[src_offs + 1] >> 2) | ((src[src_offs + 2] & 0x0F) << 4));
                dst[dst_offs + 2] = (byte)((src[src_offs + 2] >> 4) | (src[src_offs + 3] << 2));
            }

            /************************************************************************
            This function serializes boolean value into buffer

            v           boolean value to be serialized
            buf         buffer, at least 11 characters wide
            offs        offset in the buffer
        
            after return from this function, offs points to the char's past the value
            being read.
            ************************************************************************/
            private static void bool2str(bool v, char[] buf, ref int offs)
            {
                char c = v ? '1' : '0';
                int i;
                for (i = 0; i < SER_ENTRY_LENGTH; i++)
                    buf[offs + i] = c;
                offs += SER_ENTRY_LENGTH;
            }

            /************************************************************************
            This function unserializes boolean value from buffer

            buf         buffer which contains value; leading spaces/tabs/newlines are 
                        ignored, traling spaces/tabs/newlines are treated as  end  of
                        the boolean value.
            offs        offset in the buffer
        
            after return from this function, offs points to the char's past the value
            being read.

            This function raises an error in case unexpected symbol is found
            ************************************************************************/
            private static bool str2bool(char[] buf, ref int offs)
            {
                bool was0, was1;
                // string emsg = "ALGLIB: unable to read boolean value from stream";

                was0 = false;
                was1 = false;
                while (buf[offs] == ' ' || buf[offs] == '\t' || buf[offs] == '\n' || buf[offs] == '\r')
                    offs++;
                while (buf[offs] != ' ' && buf[offs] != '\t' && buf[offs] != '\n' && buf[offs] != '\r' && buf[offs] != 0)
                {
                    if (buf[offs] == '0')
                    {
                        was0 = true;
                        offs++;
                        continue;
                    }
                    if (buf[offs] == '1')
                    {
                        was1 = true;
                        offs++;
                        continue;
                    }
                    //  throw new alglib.alglibexception(emsg);
                }
                if ((!was0) && (!was1))
                {
                    // throw new alglib.alglibexception(emsg);
                }
                if (was0 && was1)
                {
                    // throw new alglib.alglibexception(emsg);
                }

                return was1 ? true : false;
            }

            /************************************************************************
            This function serializes integer value into buffer

            v           integer value to be serialized
            buf         buffer, at least 11 characters wide 
            offs        offset in the buffer
        
            after return from this function, offs points to the char's past the value
            being read.

            This function raises an error in case unexpected symbol is found
            ************************************************************************/
            private static void int2str(int v, char[] buf, ref int offs)
            {
                int i;
                byte[] _bytes = System.BitConverter.GetBytes((int)v);
                byte[] bytes = new byte[9];
                int[] sixbits = new int[12];
                byte c;

                //
                // copy v to array of bytes, sign extending it and 
                // converting to little endian order. Additionally, 
                // we set 9th byte to zero in order to simplify 
                // conversion to six-bit representation
                //
                if (!System.BitConverter.IsLittleEndian)
                    System.Array.Reverse(_bytes);
                c = v < 0 ? (byte)0xFF : (byte)0x00;
                for (i = 0; i < sizeof(int); i++)
                    bytes[i] = _bytes[i];
                for (i = sizeof(int); i < 8; i++)
                    bytes[i] = c;
                bytes[8] = 0;

                //
                // convert to six-bit representation, output
                //
                // NOTE: last 12th element of sixbits is always zero, we do not output it
                //
                threebytes2foursixbits(bytes, 0, sixbits, 0);
                threebytes2foursixbits(bytes, 3, sixbits, 4);
                threebytes2foursixbits(bytes, 6, sixbits, 8);
                for (i = 0; i < SER_ENTRY_LENGTH; i++)
                    buf[offs + i] = sixbits2char(sixbits[i]);
                offs += SER_ENTRY_LENGTH;
            }

            /************************************************************************
            This function unserializes integer value from string

            buf         buffer which contains value; leading spaces/tabs/newlines are 
                        ignored, traling spaces/tabs/newlines are treated as  end  of
                        the integer value.
            offs        offset in the buffer
        
            after return from this function, offs points to the char's past the value
            being read.

            This function raises an error in case unexpected symbol is found
            ************************************************************************/
            private static int str2int(char[] buf, ref int offs)
            {
                //string emsg =       "ALGLIB: unable to read integer value from stream";
                //string emsg3264 =   "ALGLIB: unable to read integer value from stream (value does not fit into 32 bits)";
                int[] sixbits = new int[12];
                byte[] bytes = new byte[9];
                byte[] _bytes = new byte[sizeof(int)];
                int sixbitsread, i;
                byte c;

                // 
                // 1. skip leading spaces
                // 2. read and decode six-bit digits
                // 3. set trailing digits to zeros
                // 4. convert to little endian 64-bit integer representation
                // 5. check that we fit into int
                // 6. convert to big endian representation, if needed
                //
                sixbitsread = 0;
                while (buf[offs] == ' ' || buf[offs] == '\t' || buf[offs] == '\n' || buf[offs] == '\r')
                    offs++;
                while (buf[offs] != ' ' && buf[offs] != '\t' && buf[offs] != '\n' && buf[offs] != '\r' && buf[offs] != 0)
                {
                    int d;
                    d = char2sixbits(buf[offs]);
                    if (d < 0 || sixbitsread >= SER_ENTRY_LENGTH)
                    {
                        // throw new alglib.alglibexception(emsg);
                    }
                    sixbits[sixbitsread] = d;
                    sixbitsread++;
                    offs++;
                }
                if (sixbitsread == 0)
                {
                    // throw new alglib.alglibexception(emsg);
                }
                for (i = sixbitsread; i < 12; i++)
                    sixbits[i] = 0;
                foursixbits2threebytes(sixbits, 0, bytes, 0);
                foursixbits2threebytes(sixbits, 4, bytes, 3);
                foursixbits2threebytes(sixbits, 8, bytes, 6);
                c = (bytes[sizeof(int) - 1] & 0x80) != 0 ? (byte)0xFF : (byte)0x00;
                for (i = sizeof(int); i < 8; i++)
                    if (bytes[i] != c)
                    {
                        //   throw new alglib.alglibexception(emsg3264);
                    }
                for (i = 0; i < sizeof(int); i++)
                    _bytes[i] = bytes[i];
                if (!System.BitConverter.IsLittleEndian)
                    System.Array.Reverse(_bytes);
                return System.BitConverter.ToInt32(_bytes, 0);
            }


            /************************************************************************
            This function serializes double value into buffer

            v           double value to be serialized
            buf         buffer, at least 11 characters wide 
            offs        offset in the buffer
        
            after return from this function, offs points to the char's past the value
            being read.
            ************************************************************************/
            private static void double2str(double v, char[] buf, ref int offs)
            {
                int i;
                int[] sixbits = new int[12];
                byte[] bytes = new byte[9];

                //
                // handle special quantities
                //
                if (System.Double.IsNaN(v))
                {
                    buf[offs + 0] = '.';
                    buf[offs + 1] = 'n';
                    buf[offs + 2] = 'a';
                    buf[offs + 3] = 'n';
                    buf[offs + 4] = '_';
                    buf[offs + 5] = '_';
                    buf[offs + 6] = '_';
                    buf[offs + 7] = '_';
                    buf[offs + 8] = '_';
                    buf[offs + 9] = '_';
                    buf[offs + 10] = '_';
                    offs += SER_ENTRY_LENGTH;
                    return;
                }
                if (System.Double.IsPositiveInfinity(v))
                {
                    buf[offs + 0] = '.';
                    buf[offs + 1] = 'p';
                    buf[offs + 2] = 'o';
                    buf[offs + 3] = 's';
                    buf[offs + 4] = 'i';
                    buf[offs + 5] = 'n';
                    buf[offs + 6] = 'f';
                    buf[offs + 7] = '_';
                    buf[offs + 8] = '_';
                    buf[offs + 9] = '_';
                    buf[offs + 10] = '_';
                    offs += SER_ENTRY_LENGTH;
                    return;
                }
                if (System.Double.IsNegativeInfinity(v))
                {
                    buf[offs + 0] = '.';
                    buf[offs + 1] = 'n';
                    buf[offs + 2] = 'e';
                    buf[offs + 3] = 'g';
                    buf[offs + 4] = 'i';
                    buf[offs + 5] = 'n';
                    buf[offs + 6] = 'f';
                    buf[offs + 7] = '_';
                    buf[offs + 8] = '_';
                    buf[offs + 9] = '_';
                    buf[offs + 10] = '_';
                    offs += SER_ENTRY_LENGTH;
                    return;
                }

                //
                // process general case:
                // 1. copy v to array of chars
                // 2. set 9th byte to zero in order to simplify conversion to six-bit representation
                // 3. convert to little endian (if needed)
                // 4. convert to six-bit representation
                //    (last 12th element of sixbits is always zero, we do not output it)
                //
                byte[] _bytes = System.BitConverter.GetBytes((double)v);
                if (!System.BitConverter.IsLittleEndian)
                    System.Array.Reverse(_bytes);
                for (i = 0; i < sizeof(double); i++)
                    bytes[i] = _bytes[i];
                for (i = sizeof(double); i < 9; i++)
                    bytes[i] = 0;
                threebytes2foursixbits(bytes, 0, sixbits, 0);
                threebytes2foursixbits(bytes, 3, sixbits, 4);
                threebytes2foursixbits(bytes, 6, sixbits, 8);
                for (i = 0; i < SER_ENTRY_LENGTH; i++)
                    buf[offs + i] = sixbits2char(sixbits[i]);
                offs += SER_ENTRY_LENGTH;
            }

            /************************************************************************
            This function unserializes double value from string

            buf         buffer which contains value; leading spaces/tabs/newlines are 
                        ignored, traling spaces/tabs/newlines are treated as  end  of
                        the double value.
            offs        offset in the buffer
        
            after return from this function, offs points to the char's past the value
            being read.

            This function raises an error in case unexpected symbol is found
            ************************************************************************/
            private static double str2double(char[] buf, ref int offs)
            {
                // string emsg = "ALGLIB: unable to read double value from stream";
                int[] sixbits = new int[12];
                byte[] bytes = new byte[9];
                byte[] _bytes = new byte[sizeof(double)];
                int sixbitsread, i;


                // 
                // skip leading spaces
                //
                while (buf[offs] == ' ' || buf[offs] == '\t' || buf[offs] == '\n' || buf[offs] == '\r')
                    offs++;


                //
                // Handle special cases
                //
                if (buf[offs] == '.')
                {
                    string s = new string(buf, offs, SER_ENTRY_LENGTH);
                    if (s == ".nan_______")
                    {
                        offs += SER_ENTRY_LENGTH;
                        return System.Double.NaN;
                    }
                    if (s == ".posinf____")
                    {
                        offs += SER_ENTRY_LENGTH;
                        return System.Double.PositiveInfinity;
                    }
                    if (s == ".neginf____")
                    {
                        offs += SER_ENTRY_LENGTH;
                        return System.Double.NegativeInfinity;
                    }
                    // throw new alglib.alglibexception(emsg);
                }

                // 
                // General case:
                // 1. read and decode six-bit digits
                // 2. check that all 11 digits were read
                // 3. set last 12th digit to zero (needed for simplicity of conversion)
                // 4. convert to 8 bytes
                // 5. convert to big endian representation, if needed
                //
                sixbitsread = 0;
                while (buf[offs] != ' ' && buf[offs] != '\t' && buf[offs] != '\n' && buf[offs] != '\r' && buf[offs] != 0)
                {
                    int d;
                    d = char2sixbits(buf[offs]);
                    if (d < 0 || sixbitsread >= SER_ENTRY_LENGTH)
                    {
                        //   throw new alglib.alglibexception(emsg);
                    }
                    sixbits[sixbitsread] = d;
                    sixbitsread++;
                    offs++;
                }
                if (sixbitsread != SER_ENTRY_LENGTH)
                {
                    //   throw new alglib.alglibexception(emsg);
                }
                sixbits[SER_ENTRY_LENGTH] = 0;
                foursixbits2threebytes(sixbits, 0, bytes, 0);
                foursixbits2threebytes(sixbits, 4, bytes, 3);
                foursixbits2threebytes(sixbits, 8, bytes, 6);
                for (i = 0; i < sizeof(double); i++)
                    _bytes[i] = bytes[i];
                if (!System.BitConverter.IsLittleEndian)
                    System.Array.Reverse(_bytes);
                return System.BitConverter.ToDouble(_bytes, 0);
            }
        }
    }
}
