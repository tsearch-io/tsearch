import { match } from 'ts-pattern';
import { forwardRef, ComponentPropsWithRef } from 'react';
import { ImSpinner2 } from 'react-icons/im';

import clsxm from '@/lib/clsxm';

type Variant = 'primary' | 'outline' | 'ghost' | 'light' | 'dark';

type Props = {
  isLoading?: boolean;
  isDarkBg?: boolean;
  variant?: Variant;
} & ComponentPropsWithRef<'button'>;

export const Button = forwardRef<HTMLButtonElement, Props>(
  (
    {
      children,
      className,
      disabled: buttonDisabled,
      isLoading,
      variant = 'primary',
      isDarkBg = false,
      ...rest
    },
    ref
  ) => {
    const disabled = isLoading || buttonDisabled;

    return (
      <button
        ref={ref}
        type='button'
        disabled={disabled}
        className={clsxm(
          'inline-flex items-center px-4 py-2 font-semibold',
          'focus:outline-none focus-visible:ring',
          'font-mono',
          match(variant)
            .with('primary', () => [
              // HERE !!! continue with button
              'bg-gray-300 text-gray-800',
              'border-2 border-gray-600',
              'hover:bg-gray-600 hover:text-white',
              'active:bg-gray-500',
              'disabled:bg-gray-200 disabled:text-gray-400 disabled:hover:text-gray-400',
              'focus-visible:ring-gray-600',
            ])

            .with('outline', () => [
              'text-primary-500',
              'border border-primary-500',
              'hover:bg-primary-50 active:bg-primary-100 disabled:bg-primary-100',
              isDarkBg &&
                'hover:bg-gray-900 active:bg-gray-800 disabled:bg-gray-800',
            ])
            .with('ghost', () => [
              'text-primary-500',
              'shadow-none',
              'hover:bg-primary-50 active:bg-primary-100 disabled:bg-primary-100',
              isDarkBg &&
                'hover:bg-gray-900 active:bg-gray-800 disabled:bg-gray-800',
            ])
            .with('light', () => [
              'bg-white text-dark ',
              'border border-gray-300',
              'hover:bg-gray-100 hover:text-dark',
              'active:bg-white/80 disabled:bg-gray-200',
            ])
            .with('dark', () => [
              'bg-gray-900 text-white',
              'border border-gray-600',
              'hover:bg-gray-800 active:bg-gray-700 disabled:bg-gray-700',
            ])
            .exhaustive(),
          'disabled:cursor-not-allowed',
          isLoading &&
            'relative text-transparent transition-none hover:text-transparent disabled:cursor-wait',
          className
        )}
        {...rest}
      >
        {isLoading && (
          <div
            className={clsxm(
              'absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2',
              {
                'text-white': ['primary', 'dark'].includes(variant),
                'text-black': ['light'].includes(variant),
                'text-primary-500': ['outline', 'ghost'].includes(variant),
              }
            )}
          >
            <ImSpinner2 className='animate-spin' />
          </div>
        )}
        {children}
      </button>
    );
  }
);
